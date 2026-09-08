module Main (main) where

import ArgsUtils (tokenize)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

data EvaluatedResult = PrintStdOutAndContinue String | PrintStdErrAndContinue String | Exit | Continue | RedirectStdOutAndContinue String String deriving (Show)

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  args <- getLine
  evaluatedResult <- eval args
  handleEval evaluatedResult

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
  PrintStdOutAndContinue str -> printAndContinue str
  PrintStdErrAndContinue str -> printAndContinue str
  RedirectStdOutAndContinue str file -> redirectStdOutAndContinue str file
  Continue -> main
  Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
  putStrLn str
  hFlush stdout
  main

redirectStdOutAndContinue :: String -> String -> IO ()
redirectStdOutAndContinue str file = do
  writeFile file str
  main

eval :: String -> IO EvaluatedResult
eval untokenizedArgs = if null untokenizedArgs then pure Continue else modifyEvaluatedResultWithRedirectFile (eval' command args) file
  where
    tokenizedArgs = tokenize untokenizedArgs
    command = head tokenizedArgs
    (args, file) = getArgsAndRedirectFile (tail tokenizedArgs)

modifyEvaluatedResultWithRedirectFile :: IO EvaluatedResult -> Maybe String -> IO EvaluatedResult
modifyEvaluatedResultWithRedirectFile ioEvaluatedResult file = do
  evaluatedResult <- ioEvaluatedResult
  case file of
    Nothing -> ioEvaluatedResult
    Just fileName -> case evaluatedResult of
      (PrintStdOutAndContinue str) -> pure (RedirectStdOutAndContinue str fileName)
      _ -> ioEvaluatedResult

getArgsAndRedirectFile :: [String] -> ([String], Maybe String)
getArgsAndRedirectFile tokenizedArgs =
  let args = takeWhile tokenIsNotRedirectOperator tokenizedArgs
      file = getRedirectFile args tokenizedArgs
   in (args, file)

getRedirectFile :: [String] -> [String] -> Maybe String
getRedirectFile args tokenizedArgs =
  if (length args == length tokenizedArgs || null tokenizedArgs)
    then Nothing
    else case drop 1 (dropWhile tokenIsNotRedirectOperator tokenizedArgs) of
      [] -> Nothing
      val -> Just (last val)

tokenIsNotRedirectOperator :: String -> Bool
tokenIsNotRedirectOperator token = token /= ">" && token /= "1>"

eval' :: String -> [String] -> IO EvaluatedResult
eval' command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintStdOutAndContinue (unwords args)
  "pwd" -> PrintStdOutAndContinue <$> getCurrentDirectory
  "cd" -> handleChangeDirectoryCommand (unwords args)
  "type" -> handleTypeCommand (unwords args)
  otherwise -> handleUnknownCommand command args

removeLastNewline :: String -> String
removeLastNewline [] = []
removeLastNewline s
  | last s == '\n' = init s
  | otherwise = s

handleUnknownCommand :: String -> [String] -> IO EvaluatedResult
handleUnknownCommand command args = do
  str <- _findExecutable command
  if "not found" `isInfixOf` str
    then pure $ PrintStdErrAndContinue str
    else do
      (exitCode, stdOut, err) <- readProcessWithExitCode (takeFileName str) args ""
      case exitCode of
        ExitSuccess -> pure (PrintStdOutAndContinue (removeLastNewline stdOut))
        ExitFailure _ -> pure Continue

handleChangeDirectoryCommand :: String -> IO EvaluatedResult
handleChangeDirectoryCommand path = do
  homeDir <- getHomeDirectory
  let parsedPath = replaceString "~" homeDir path
  directoryExists <- doesDirectoryExist parsedPath
  case directoryExists of
    False -> pure $ PrintStdErrAndContinue ("cd: " <> parsedPath <> ": No such file or directory")
    True -> do
      setCurrentDirectory parsedPath
      pure Continue

handleTypeCommand :: String -> IO EvaluatedResult
handleTypeCommand args = case args of
  x | x `elem` ["exit", "echo", "type", "pwd", "cd"] -> pure $ PrintStdOutAndContinue (x <> " is a shell builtin")
  _ -> do
    executable <- _findExecutable args
    pure $ PrintStdOutAndContinue executable

replaceString :: String -> String -> String -> String
replaceString old new haystack =
  T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack haystack)

_findExecutable :: String -> IO String
_findExecutable args = do
  maybeFilePath <- findExecutable args
  case maybeFilePath of
    Just filePath -> pure filePath
    Nothing -> pure (args <> ": not found")
