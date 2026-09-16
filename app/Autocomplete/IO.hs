module Autocomplete.IO (InputAutoCompletionState (..), handleAutoCompletion) where

import Autocomplete.Core (AutoCompletionType (..), WasAutoCompleteMatchFound (..), findBuiltInAutoCompleteMatch, findBuiltInAutoCompleteMatch', findLongestCommonPrefix, getAutoCompletionType, getFileNameFromInputSoFar)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Data.List (isInfixOf, isPrefixOf, maximumBy)
import Data.Ord (comparing)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering)

data InputAutoCompletionState = Normal | OneTabPressed [String]

handleAutoCompletion :: String -> InputAutoCompletionState -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleAutoCompletion inputSoFar inputAutoCompletionSate getInput' = case inputAutoCompletionSate of
  Normal -> case getAutoCompletionType inputSoFar of
    CommandAutoCompletion -> handleCommandNormalAutoCompletionState inputSoFar getInput'
    FilenameAutoCompletion -> handleFilenameNormalAutoCompletionState inputSoFar getInput'
  (OneTabPressed options) -> handleOneTabAutoCompletionState inputSoFar options getInput'

handleFilenameNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleFilenameNormalAutoCompletionState inputSoFar getInput' = do
  wasFilenameAutoCompleteMatchFound <- findFilenameAutoCompleteMatch $ getFileNameFromInputSoFar inputSoFar
  case wasFilenameAutoCompleteMatchFound of
    NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
    (AutoCompleteMatchFound fileNameAutoCompleteMatch) -> handleAutoCompleteFound (unwords (init $ words inputSoFar) ++ " " ++ fileNameAutoCompleteMatch) getInput'
    (AutoCompleteMatchesFound fileNameAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar fileNameAutoCompleteMatches getInput'

handleCommandNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleCommandNormalAutoCompletionState inputSoFar getInput' = do
  case findBuiltInAutoCompleteMatch inputSoFar of
    NoAutoCompleteMatchFound -> do
      wasExecutableAutoCompleteMatchFound <- findExecutableAutoCompleteMatch inputSoFar
      case wasExecutableAutoCompleteMatchFound of
        NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
        (AutoCompleteMatchFound executableAutoCompleteMatch) -> handleAutoCompleteFound executableAutoCompleteMatch getInput'
        (AutoCompleteMatchesFound executableAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar executableAutoCompleteMatches getInput'
    (AutoCompleteMatchFound builtInAutoCompleteMatch) -> handleAutoCompleteFound builtInAutoCompleteMatch getInput'
    (AutoCompleteMatchesFound builtInAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar builtInAutoCompleteMatches getInput'

handleAutoCompleteMatchesFound :: String -> [String] -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleAutoCompleteMatchesFound inputSoFar matches getInput' = do
  putStr ['\a']
  hFlush stdout
  getInput' inputSoFar (OneTabPressed matches)

handleOneTabAutoCompletionState :: String -> [String] -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleOneTabAutoCompletionState inputSoFar options getInput' = do
  putStr ['\n']
  hFlush stdout
  putStrLn (unwords options)
  hFlush stdout
  putStr ("$ " ++ inputSoFar)
  hFlush stdout
  getInput' inputSoFar Normal

handleNoAutoCompleteFound :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleNoAutoCompleteFound inputSoFar getInput' = do
  putStr ['\a']
  hFlush stdout
  getInput' inputSoFar Normal

handleAutoCompleteFound :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleAutoCompleteFound inferredCommand getInput' = do
  clearFromCursorToLineBeginning
  setCursorColumn 0
  putStr ("$ " ++ inferredCommand)
  hFlush stdout
  getInput' inferredCommand Normal

getAllExecutablesInDir :: String -> IO [String]
getAllExecutablesInDir dir = do
  directoryExists <- doesDirectoryExist dir
  case directoryExists of
    False -> pure []
    True -> do
      files <- listDirectory dir
      filterM isFileExecutable ((map (\file -> dir ++ [pathSeparator] ++ file)) files)

isFileExecutable :: String -> IO Bool
isFileExecutable file = do
  result <- try (getPermissions file) :: IO (Either IOError Permissions)
  case result of
    Right permissions -> pure $ executable permissions
    Left err -> pure False

getAllExecutablesInDirs :: [String] -> IO [String]
getAllExecutablesInDirs dirs = do
  allExecs <- mapM getAllExecutablesInDir dirs
  pure $ (map takeBaseName (concat allExecs))

findExecutableAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findExecutableAutoCompleteMatch partialCommand = do
  execSearchPath <- getSearchPath
  allExecutables <- getAllExecutablesInDirs execSearchPath
  case findBuiltInAutoCompleteMatch' partialCommand allExecutables of
    NoAutoCompleteMatchFound -> pure NoAutoCompleteMatchFound
    (AutoCompleteMatchFound match) -> pure (AutoCompleteMatchFound match)
    (AutoCompleteMatchesFound matches) -> case (findLongestCommonPrefix matches) of
      Nothing -> pure (AutoCompleteMatchesFound matches)
      Just prefix -> pure (AutoCompleteMatchFound prefix)

findFilenameAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findFilenameAutoCompleteMatch partialFileName = do
  allFiles <- listDirectory "."
  case findBuiltInAutoCompleteMatch' partialFileName allFiles of
    NoAutoCompleteMatchFound -> pure NoAutoCompleteMatchFound
    (AutoCompleteMatchFound match) -> pure (AutoCompleteMatchFound match)
    (AutoCompleteMatchesFound matches) -> case (findLongestCommonPrefix matches) of
      Nothing -> pure (AutoCompleteMatchesFound matches)
      Just prefix -> pure (AutoCompleteMatchFound prefix)
