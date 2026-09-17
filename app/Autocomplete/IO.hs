module Autocomplete.IO (InputAutoCompletionState (..), handleAutoCompletion) where

import Autocomplete.Core (AutoCompletionType (..), FileNameAutoCompletionType (..), WasAutoCompleteMatchFound (..), findAutoCompleteMatch, findBuiltInAutoCompleteMatch, findLongestCommonPrefix, getAutoCompletionType, getFileNameAutoCompletionType, getFileNameFromInputSoFar, getFileNameFromPartialNestedFileName, getPathFromPartialNestedFileName)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Data.List (intercalate, isInfixOf, isPrefixOf, maximumBy, sort)
import Data.Ord (comparing)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering)

data InputAutoCompletionState = Normal | OneTabPressed [String]

data PathType = PathIsFile | PathIsDirectory

handleAutoCompletion :: String -> InputAutoCompletionState -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleAutoCompletion inputSoFar inputAutoCompletionSate getInput' = case inputAutoCompletionSate of
  Normal -> case getAutoCompletionType inputSoFar of
    CommandAutoCompletion -> handleCommandNormalAutoCompletionState inputSoFar getInput'
    FileNameAutoCompletion -> case getFileNameAutoCompletionType inputSoFar of
      NonNestedFileNameAutoCompletion -> handleFileNameNonNestedNormalAutoCompletionState inputSoFar getInput'
      NestedFileNameAutoCompletion -> handleFileNameNestedNormalAutoCompletionState inputSoFar getInput'
  (OneTabPressed options) -> handleOneTabAutoCompletionState inputSoFar options getInput'

handleFileNameNestedNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleFileNameNestedNormalAutoCompletionState inputSoFar getInput' = do
  wasFileNameAutoCompleteMatchFound <- findNestedFileNameAutoCompleteMatch $ getFileNameFromInputSoFar inputSoFar
  case wasFileNameAutoCompleteMatchFound of
    NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
    (AutoCompleteMatchFound fileNameAutoCompleteMatch) ->
      let filePathPartial = (getPathFromPartialNestedFileName $ getFileNameFromInputSoFar inputSoFar) ++ [pathSeparator] ++ fileNameAutoCompleteMatch
       in do
            filePath <- addPathSeparatorIfDirectory filePathPartial
            handleAutoCompleteFound (unwords (init $ words inputSoFar) ++ " " ++ filePath) getInput'
    (AutoCompleteMatchesFound fileNameAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar fileNameAutoCompleteMatches getInput'

handleFileNameNonNestedNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
handleFileNameNonNestedNormalAutoCompletionState inputSoFar getInput' = do
  wasFileNameAutoCompleteMatchFound <- findFileNameAutoCompleteMatch $ getFileNameFromInputSoFar inputSoFar
  case wasFileNameAutoCompleteMatchFound of
    NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
    (AutoCompleteMatchFound fileNameAutoCompleteMatch) -> do
      filePath <- addPathSeparatorIfDirectory fileNameAutoCompleteMatch
      handleAutoCompleteFound (unwords (init $ words inputSoFar) ++ " " ++ filePath) getInput'
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
  putStrLn (intercalate "  " (sort options))
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
  putStr ("$ " ++ inferredCommand ++ " ")
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
  findAutoCompleteMatchIO partialCommand allExecutables

findFileNameAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findFileNameAutoCompleteMatch partialFileName = do
  allFiles <- listDirectory "."
  findAutoCompleteMatchIO partialFileName allFiles

findNestedFileNameAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findNestedFileNameAutoCompleteMatch partialNestedFileName =
  let path = getPathFromPartialNestedFileName partialNestedFileName
      partialFileName = getFileNameFromPartialNestedFileName partialNestedFileName
   in do
        allFiles <- listDirectory ("." ++ [pathSeparator] ++ path)
        if partialFileName == "" then findAutoCompleteMatchIO (head allFiles) allFiles else findAutoCompleteMatchIO partialFileName allFiles

findAutoCompleteMatchIO :: String -> [String] -> IO WasAutoCompleteMatchFound
findAutoCompleteMatchIO partial options =
  case findAutoCompleteMatch partial options of
    NoAutoCompleteMatchFound -> pure NoAutoCompleteMatchFound
    (AutoCompleteMatchFound match) -> pure (AutoCompleteMatchFound match)
    (AutoCompleteMatchesFound matches) -> case (findLongestCommonPrefix matches) of
      Nothing -> pure (AutoCompleteMatchesFound matches)
      Just prefix -> pure (AutoCompleteMatchFound prefix)

getPathType :: String -> IO PathType
getPathType path = do
  isFile <- doesFileExist path
  if isFile then pure PathIsFile else pure PathIsDirectory

addPathSeparatorIfDirectory :: String -> IO String
addPathSeparatorIfDirectory path = do
  pathType <- getPathType path
  case pathType of
    PathIsDirectory -> pure $ path ++ [pathSeparator]
    PathIsFile -> pure path
