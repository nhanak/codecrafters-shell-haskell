module Main (main) where

import ArgsUtils (getArgs, getCommand)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.FilePath (takeFileName)
import System.IO (hFlush, stdout)
import System.Process (callProcess)

data EvaluatedResult = PrintAndContinue String | Exit | Continue

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  args <- getLine
  evaluatedResult <- eval args
  handleEval evaluatedResult

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

eval :: String -> IO EvaluatedResult
eval args = if null args then pure Continue else eval' (getCommand args) (getArgs args)

eval' :: String -> [String] -> IO EvaluatedResult
eval' command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintAndContinue (unwords args)
  "pwd" -> PrintAndContinue <$> getCurrentDirectory
  "cd" -> handleChangeDirectoryCommand (unwords args)
  "type" -> handleTypeCommand (unwords args)
  otherwise -> handleUnknownCommand command args

handleUnknownCommand :: String -> [String] -> IO EvaluatedResult
handleUnknownCommand command args = do
  str <- _findExecutable command
  if "not found" `isInfixOf` str
    then pure $ PrintAndContinue str
    else do
      callProcess (takeFileName str) args
      pure Continue

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

handleTypeCommand :: String -> IO EvaluatedResult
handleTypeCommand args = case args of
  x | x `elem` ["exit", "echo", "type", "pwd", "cd"] -> pure $ PrintAndContinue (x <> " is a shell builtin")
  _ -> do
    executable <- _findExecutable args
    pure $ PrintAndContinue executable

replaceString :: String -> String -> String -> String
replaceString old new haystack =
  T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack haystack)

_findExecutable :: String -> IO String
_findExecutable args = do
  maybeFilePath <- findExecutable args
  case maybeFilePath of
    Just filePath -> pure filePath
    Nothing -> pure (args <> ": not found")
