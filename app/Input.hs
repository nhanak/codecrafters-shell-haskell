module Input (getInput) where

import Autocomplete.IO (InputAutoCompletionState (..), handleAutoCompletion)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Data.List (isInfixOf, isPrefixOf, maximumBy)
import Data.Ord (comparing)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering)

getInput :: IO String
getInput = getInput' "" Normal

getInput' :: String -> InputAutoCompletionState -> IO String
getInput' inputSoFar inputAutoCompletionState = do
  char <- getCharNoBuffering
  case char of
    '\b' ->
      if null inputSoFar
        then getInput' inputSoFar Normal
        else do
          clearFromCursorToLineBeginning
          setCursorColumn 0
          putStr ("$ " ++ (init inputSoFar))
          hFlush stdout
          getInput' (init inputSoFar) Normal
    '\r' -> do
      putStr [char, '\n']
      hFlush stdout
      pure (inputSoFar ++ ['\r'])
    '\n' -> do
      putStr [char]
      hFlush stdout
      pure (inputSoFar ++ ['\n'])
    '\t' -> handleAutoCompletion inputSoFar inputAutoCompletionState getInput'
    _ -> do
      putStr [char]
      hFlush stdout
      getInput' (inputSoFar ++ [char]) Normal
