module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.FilePath (takeFileName)
import System.IO (hFlush, stdout)
import System.Process (callProcess)

data EvaluatedResult = PrintAndContinue String | Exit | Continue

splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = [""]
splitOnChar c xs =
  case break (== c) xs of
    (left, "") -> [left]
    (left, _ : right) -> left : splitOnChar c right

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  args <- read'
  evaluatedResult <- eval args
  handleEval evaluatedResult

read' :: IO String
read' = getLine

eval :: String -> IO EvaluatedResult
eval args = if null args then pure Continue else eval' (getCommand args) (getRemainingArgs' args)

eval' :: String -> String -> IO EvaluatedResult
eval' command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintAndContinue args
  "pwd" -> PrintAndContinue <$> getCurrentDirectory
  "cd" -> handleChangeDirectoryCommand args
  "type" -> do
    str <- handleTypeCommand args
    pure $ PrintAndContinue str
  otherwise -> do
    str <- _findExecutable command
    if "not found" `isInfixOf` str
      then pure $ PrintAndContinue str
      else do
        callProcess (takeFileName str) (words args)
        pure Continue

-- args is one big string
-- args can have values enclosed between ''
-- treating args as a str is not going to work, to much random processing in random locations
-- should treat args as [String]
-- need to change getRemainingArgs

replaceString :: String -> String -> String -> String
replaceString old new haystack =
  T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack haystack)

getAllSubstrings :: Char -> Char -> String -> [String]
getAllSubstrings _ _ [] = []
getAllSubstrings start end str =
  let initial = takeWhile (/= start) str
   in [initial] ++ getAllSubstrings start end (safeTail (dropWhile (/= start) str))

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

handleChangeDirectoryCommand :: String -> IO EvaluatedResult
handleChangeDirectoryCommand path = do
  homeDir <- getHomeDirectory
  let parsedPath = replaceString "~" homeDir path
  directoryExists <- doesDirectoryExist parsedPath
  case directoryExists of
    False -> pure $ PrintAndContinue ("cd: " <> parsedPath <> ": No such file or directory")
    True -> do
      setCurrentDirectory parsedPath
      pure Continue

handleTypeCommand :: String -> IO String
handleTypeCommand args = case args of
  x | x `elem` ["exit", "echo", "type", "pwd", "cd"] -> pure $ x <> " is a shell builtin"
  _ -> _findExecutable args

_findExecutable :: String -> IO String
_findExecutable args = do
  maybeFilePath <- findExecutable args
  case maybeFilePath of
    Just filePath -> pure filePath
    Nothing -> pure (args <> ": not found")

getCommand :: String -> String
getCommand args = head (words args)

-- getRemainingArgs :: String -> String
-- getRemainingArgs args = unwords (tail $ words args)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

getRemainingArgs' :: String -> String
getRemainingArgs' args =
  let (first, rest) = break (== ' ') args
   in unwords $ filter (/= "") (getAllSubstrings '\'' '\'' (trim rest))

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
  PrintAndContinue str -> printAndContinue str
  Continue -> main
  Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
  putStrLn str
  hFlush stdout
  main
