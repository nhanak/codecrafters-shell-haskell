module Input (getInput) where

import Autocomplete.IO (InputAutoCompletionState (..), handleAutoCompletion)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Control.Monad.State
import Core (CompleterScript (..), ShellState (..), io)
import Data.List (isInfixOf, isPrefixOf, maximumBy)
import Data.Ord (comparing)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering)

getInput :: StateT ShellState IO String
getInput = getInput' "" Normal

getInput' :: String -> InputAutoCompletionState -> StateT ShellState IO String
getInput' inputSoFar inputAutoCompletionState = do
  char <- io $ getCharNoBuffering
  case char of
    '\b' ->
      if null inputSoFar
        then getInput' inputSoFar Normal
        else do
          io $ clearFromCursorToLineBeginning
          io $ setCursorColumn 0
          io $ putStr ("$ " ++ (init inputSoFar))
          io $ hFlush stdout
          getInput' (init inputSoFar) Normal
    '\r' -> do
      io $ putStr [char, '\n']
      io $ hFlush stdout
      io $ pure (inputSoFar ++ ['\r'])
    '\n' -> do
      io $ putStr [char]
      io $ hFlush stdout
      io $ pure (inputSoFar ++ ['\n'])
    '\t' -> handleAutoCompletion inputSoFar inputAutoCompletionState getInput'
    _ -> do
      io $ putStr [char]
      io $ hFlush stdout
      getInput' (inputSoFar ++ [char]) Normal
