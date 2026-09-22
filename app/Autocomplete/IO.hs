module Autocomplete.IO (InputAutoCompletionState (..), handleAutoCompletion) where

import Autocomplete.Core (AutoCompletionType (..), FileNameAutoCompletionType (..), WasAutoCompleteMatchFound (..), addSpaceIfNotDirectory, findAutoCompleteMatch, findBuiltInAutoCompleteMatch, findLongestCommonPrefix, getAutoCompletionType, getFileNameAutoCompletionType, getFileNameFromInputSoFar, getFileNameFromPartialNestedFileName, getInputBeforeFilePath, getPathFromPartialNestedFileName, onlyOneOptionMatchesPrefix, pathIsDirectoryLike)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Control.Monad.State
import Core (CompleterScript (..), ShellState (..), getCompleterScript, getCompleterScriptCommands, io)
import Data.List (intercalate, isInfixOf, isPrefixOf, maximumBy, sort)
import Data.Ord (comparing)
import Debug.Trace (traceShow)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, stdin, stdout)
import System.Process (readProcess)

data InputAutoCompletionState = Normal | OneTabPressed [String] deriving (Show)

data PathType = PathIsFile | PathIsDirectory deriving (Show)

handleAutoCompletion :: String -> InputAutoCompletionState -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleAutoCompletion inputSoFar inputAutoCompletionSate getInput' = case inputAutoCompletionSate of
  Normal -> do
    completerScriptCommands <- getCompleterScriptCommands
    case getAutoCompletionType inputSoFar completerScriptCommands of
      CommandAutoCompletion -> handleCommandNormalAutoCompletionState inputSoFar getInput'
      CompleterScriptAutoCompletion -> handleCompleterScriptNormalAutoCompletionState inputSoFar getInput'
      FileNameAutoCompletion -> case getFileNameAutoCompletionType inputSoFar of
        NonNestedFileNameAutoCompletion -> handleFileNameNonNestedNormalAutoCompletionState inputSoFar getInput'
        NestedFileNameAutoCompletion -> handleFileNameNestedNormalAutoCompletionState inputSoFar getInput'
  (OneTabPressed options) -> handleOneTabAutoCompletionState inputSoFar options getInput'

handleCompleterScriptNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleCompleterScriptNormalAutoCompletionState inputSoFar getInput' = do
  maybeCompleterScript <- getCompleterScript (head (words inputSoFar))
  case maybeCompleterScript of
    Nothing -> handleNoAutoCompleteFound inputSoFar getInput'
    Just completerScript -> do
      out <- io $ readProcess (path completerScript) [] ""
      handleAutoCompleteFound (inputSoFar ++ out ++ [' '] ++ "u") getInput'

handleFileNameNestedNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleFileNameNestedNormalAutoCompletionState inputSoFar getInput' = do
  wasFileNameAutoCompleteMatchFound <- io $ findNestedFileNameAutoCompleteMatch $ getFileNameFromInputSoFar inputSoFar
  case wasFileNameAutoCompleteMatchFound of
    NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
    (AutoCompleteMatchFound fileNameAutoCompleteMatch) ->
      let filePath = (getPathFromPartialNestedFileName $ getFileNameFromInputSoFar inputSoFar) ++ [pathSeparator] ++ fileNameAutoCompleteMatch
       in handleAutoCompleteFound (getInputBeforeFilePath inputSoFar ++ " " ++ filePath) getInput'
    (AutoCompleteMatchesFound fileNameAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar fileNameAutoCompleteMatches getInput'

handleFileNameNonNestedNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleFileNameNonNestedNormalAutoCompletionState inputSoFar getInput' = do
  wasFileNameAutoCompleteMatchFound <- io $ findFileNameAutoCompleteMatch $ getFileNameFromInputSoFar inputSoFar
  case wasFileNameAutoCompleteMatchFound of
    NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
    (AutoCompleteMatchFound fileNameAutoCompleteMatch) -> handleAutoCompleteFound (getInputBeforeFilePath inputSoFar ++ " " ++ fileNameAutoCompleteMatch) getInput'
    (AutoCompleteMatchesFound fileNameAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar fileNameAutoCompleteMatches getInput'

-- here for sure
-- handleCommandNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> IO String) -> StateT ShellState IO String
-- handleCommandNormalAutoCompletionState inputSoFar getInput' = do
--  case findBuiltInAutoCompleteMatch inputSoFar of
--    NoAutoCompleteMatchFound -> do
--      wasExecutableAutoCompleteMatchFound <- findExecutableAutoCompleteMatch inputSoFar
--      case wasExecutableAutoCompleteMatchFound of
--        NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar getInput'
--       (AutoCompleteMatchFound executableAutoCompleteMatch) -> handleAutoCompleteFound executableAutoCompleteMatch getInput'
--        (AutoCompleteMatchesFound executableAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar executableAutoCompleteMatches getInput'
--    (AutoCompleteMatchFound builtInAutoCompleteMatch) -> handleAutoCompleteFound builtInAutoCompleteMatch getInput'
--   (AutoCompleteMatchesFound builtInAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar builtInAutoCompleteMatches getInput'

handleCommandNormalAutoCompletionState :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleCommandNormalAutoCompletionState inputSoFar getInput' = findAutoCompleteMatchOrContinue inputSoFar getInput' findBuiltInAutoCompleteMatchIO whenNoBuiltInAutoCompleteMatchFound

findBuiltInAutoCompleteMatchIO :: String -> StateT ShellState IO WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatchIO inputSoFar = pure $ findBuiltInAutoCompleteMatch inputSoFar

whenNoBuiltInAutoCompleteMatchFound :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
whenNoBuiltInAutoCompleteMatchFound inputSoFar getInput' = findAutoCompleteMatchOrContinue inputSoFar getInput' findExecutableAutoCompleteMatch whenNoExecutableAutoCompleteMatchFound

-- whenNoBuiltInAutoCompleteMatchFound :: String -> (String -> InputAutoCompletionState -> IO String) -> IO String
-- whenNoBuiltInAutoCompleteMatchFound inputSoFar getInput' = findAutoCompleteMatchOrContinue inputSoFar getInput' findCompleterScriptAutoCompleteMatch whenNoCompleterScriptAutoCompleteMatchFound

-- findCompleterScriptAutoCompleteMatch :: String -> StateT ShellState IO WasAutoCompleteMatchFound
-- findCompleterScriptAutoCompleteMatch inputSoFar = do
--  completerScript <- getCompleterScript inputSoFar
--  io $ putStrLn ("inputSoFar: " ++ inputSoFar)
--  case completerScript of
--    Nothing -> pure NoAutoCompleteMatchFound
--    Just x -> pure $ AutoCompleteMatchFound (path x)

-- whenNoCompleterScriptAutoCompleteMatchFound :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
-- whenNoCompleterScriptAutoCompleteMatchFound inputSoFar getInput' = findAutoCompleteMatchOrContinue inputSoFar getInput' findExecutableAutoCompleteMatch whenNoExecutableAutoCompleteMatchFound

whenNoExecutableAutoCompleteMatchFound :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
whenNoExecutableAutoCompleteMatchFound = handleNoAutoCompleteFound

findAutoCompleteMatchOrContinue :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> (String -> StateT ShellState IO WasAutoCompleteMatchFound) -> (String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String) -> StateT ShellState IO String
findAutoCompleteMatchOrContinue inputSoFar getInput' findAutoCompleteMatch whenNoAutoCompleteMatchFound = do
  autoCompleteMatch <- findAutoCompleteMatch inputSoFar
  case autoCompleteMatch of
    NoAutoCompleteMatchFound -> whenNoAutoCompleteMatchFound inputSoFar getInput'
    (AutoCompleteMatchFound match) -> handleAutoCompleteFound match getInput'
    (AutoCompleteMatchesFound matches) -> handleAutoCompleteMatchesFound inputSoFar matches getInput'

handleAutoCompleteMatchesFound :: String -> [String] -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleAutoCompleteMatchesFound inputSoFar matches getInput' = do
  io $ putStr ['\a']
  io $ hFlush stdout
  getInput' inputSoFar (OneTabPressed matches)

handleOneTabAutoCompletionState :: String -> [String] -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleOneTabAutoCompletionState inputSoFar options getInput' = do
  io $ putStr ['\n']
  io $ hFlush stdout
  io $ putStrLn (intercalate "  " (sort options))
  io $ hFlush stdout
  io $ putStr ("$ " ++ inputSoFar)
  io $ hFlush stdout
  getInput' inputSoFar Normal

handleNoAutoCompleteFound :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleNoAutoCompleteFound inputSoFar getInput' = do
  io $ putStr ['\a']
  io $ hFlush stdout
  getInput' inputSoFar Normal

handleAutoCompleteFound :: String -> (String -> InputAutoCompletionState -> StateT ShellState IO String) -> StateT ShellState IO String
handleAutoCompleteFound inferredCommand getInput' = do
  io $ clearFromCursorToLineBeginning
  io $ setCursorColumn 0
  io $ putStr ("$ " ++ inferredCommand)
  io $ hFlush stdout
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

findExecutableAutoCompleteMatch :: String -> StateT ShellState IO WasAutoCompleteMatchFound
findExecutableAutoCompleteMatch partialCommand = do
  execSearchPath <- io $ getSearchPath
  allExecutables <- io $ getAllExecutablesInDirs execSearchPath
  io $ findAutoCompleteMatchIO partialCommand allExecutables

findFileNameAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findFileNameAutoCompleteMatch partialFileName =
  let root = "." ++ [pathSeparator]
   in findNestedFileNameAutoCompleteMatch' partialFileName root

findNestedFileNameAutoCompleteMatch :: String -> IO WasAutoCompleteMatchFound
findNestedFileNameAutoCompleteMatch partialNestedFileName =
  let path = getPathFromPartialNestedFileName partialNestedFileName
      root = "." ++ [pathSeparator] ++ path ++ [pathSeparator]
      partialFileName = getFileNameFromPartialNestedFileName partialNestedFileName
   in findNestedFileNameAutoCompleteMatch' partialFileName root

findNestedFileNameAutoCompleteMatch' :: String -> String -> IO WasAutoCompleteMatchFound
findNestedFileNameAutoCompleteMatch' partialFileName root = do
  allFiles <- listDirectory root
  allFilesWithExtensions <- addPathSeparatorToDirectories root allFiles
  if partialFileName == "" && length allFilesWithExtensions > 0
    then case length allFilesWithExtensions of
      1 -> pure (AutoCompleteMatchFound (addSpaceIfNotDirectory $ head allFilesWithExtensions))
      _ -> pure (AutoCompleteMatchesFound allFilesWithExtensions)
    else do
      ans <- (findAutoCompleteMatchIO partialFileName allFiles)
      case ans of
        NoAutoCompleteMatchFound -> pure NoAutoCompleteMatchFound
        (AutoCompleteMatchFound match) ->
          let matchWithoutLastSpace = if last match == ' ' then init match else match
           in if onlyOneOptionMatchesPrefix matchWithoutLastSpace allFilesWithExtensions
                then do
                  ans2 <- addPathSeparatorIfDirectory root matchWithoutLastSpace
                  if last ans2 == pathSeparator then pure $ AutoCompleteMatchFound ans2 else pure $ AutoCompleteMatchFound match
                else pure $ AutoCompleteMatchFound match
        (AutoCompleteMatchesFound matches) -> do
          matchesWithExtensions <- addPathSeparatorToDirectories root matches
          pure $ AutoCompleteMatchesFound matchesWithExtensions

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
  isDirectory <- doesDirectoryExist path
  if isDirectory then (pure PathIsDirectory) else (pure PathIsFile)

addPathSeparatorIfDirectoryCWD :: String -> IO String
addPathSeparatorIfDirectoryCWD path = addPathSeparatorIfDirectory ("." ++ [pathSeparator]) path

addPathSeparatorIfDirectory :: String -> String -> IO String
addPathSeparatorIfDirectory root path = do
  pathType <- getPathType (root ++ path)
  case pathType of
    PathIsDirectory -> pure $ path ++ [pathSeparator]
    PathIsFile -> pure path

addPathSeparatorToDirectories :: String -> [String] -> IO [String]
addPathSeparatorToDirectories root paths = do
  mapM (addPathSeparatorIfDirectory root) paths
