module Main (main) where

import Data.List (isInfixOf)
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
eval args = if null args then pure Continue else eval' (getCommand args) (getRemainingArgs args)

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

replaceString :: String -> String -> String -> String
replaceString old new haystack =
  T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack haystack)

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
handleTypeCommand remainingArgs = case remainingArgs of
  x | x `elem` ["exit", "echo", "type", "pwd", "cd"] -> pure $ x <> " is a shell builtin"
  _ -> _findExecutable remainingArgs

_findExecutable :: String -> IO String
_findExecutable remainingArgs = do
  maybeFilePath <- findExecutable remainingArgs
  case maybeFilePath of
    Just filePath -> pure filePath
    Nothing -> pure (remainingArgs <> ": not found")

getCommand :: String -> String
getCommand args = head (words args)

getRemainingArgs :: String -> String
getRemainingArgs args = unwords (tail $ words args)

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
