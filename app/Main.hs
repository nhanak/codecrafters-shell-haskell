module Main (main) where

import ArgsUtils (tokenize)
import Control.Exception (try)
import Control.Monad (filterM, mapM)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import System.Console.ANSI
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, hSetEcho, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering, initGetCharNoBuffering)
import System.Process (readProcessWithExitCode)

data EvaluatedResult = PrintStdOutAndContinue String | PrintStdErrAndContinue String | Exit | Continue | RedirectStdOutAndContinue String String RedirectMode | RedirectStdErrAndContinue String String RedirectMode | PrintStdOutAndRedirectStdErrAndContinue String String String RedirectMode | RedirectStdOutAndPrintStdErrAndContinue String String String RedirectMode | PrintStdOutAndPrintStdErrAndContinue String String deriving (Show)

data RedirectStdToFile = RedirectStdOutToFile RedirectMode String | RedirectStdErrToFile RedirectMode String | NoRedirect deriving (Show)

data RedirectMode = Append | Overwrite deriving (Show)

data InputAutoCompletionState = Normal | OneTabPressed [String]

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
    '\t' -> handleAutoCompletion inputSoFar inputAutoCompletionState
    _ -> do
      putStr [char]
      hFlush stdout
      getInput' (inputSoFar ++ [char]) Normal

handleAutoCompletion :: String -> InputAutoCompletionState -> IO String
handleAutoCompletion inputSoFar inputAutoCompletionSate = case inputAutoCompletionSate of
  Normal -> handleNormalAutoCompletionState inputSoFar
  (OneTabPressed options) -> handleOneTabAutoCompletionState inputSoFar options

handleNormalAutoCompletionState :: String -> IO String
handleNormalAutoCompletionState inputSoFar = do
  case findBuiltInAutoCompleteMatch inputSoFar of
    NoAutoCompleteMatchFound -> do
      wasExecutableAutoCompleteMatchFound <- findExecutableAutoCompleteMatch inputSoFar
      case wasExecutableAutoCompleteMatchFound of
        NoAutoCompleteMatchFound -> handleNoAutoCompleteFound inputSoFar
        (AutoCompleteMatchFound executableAutoCompleteMatch) -> handleAutoCompleteFound executableAutoCompleteMatch
        (AutoCompleteMatchesFound executableAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar executableAutoCompleteMatches
    (AutoCompleteMatchFound builtInAutoCompleteMatch) -> handleAutoCompleteFound builtInAutoCompleteMatch
    (AutoCompleteMatchesFound builtInAutoCompleteMatches) -> handleAutoCompleteMatchesFound inputSoFar builtInAutoCompleteMatches

handleAutoCompleteMatchesFound :: String -> [String] -> IO String
handleAutoCompleteMatchesFound inputSoFar matches = do
  putStr ['\a']
  hFlush stdout
  getInput' inputSoFar (OneTabPressed matches)

handleOneTabAutoCompletionState :: String -> [String] -> IO String
handleOneTabAutoCompletionState inputSoFar options = do
  putStr ['\n']
  hFlush stdout
  putStrLn (unwords options)
  hFlush stdout
  putStr ("$ " ++ inputSoFar)
  hFlush stdout
  getInput' inputSoFar Normal

handleNoAutoCompleteFound :: String -> IO String
handleNoAutoCompleteFound inputSoFar = do
  putStr ['\a']
  hFlush stdout
  getInput' inputSoFar Normal

handleAutoCompleteFound :: String -> IO String
handleAutoCompleteFound inferredCommand = do
  clearFromCursorToLineBeginning
  setCursorColumn 0
  putStr ("$ " ++ inferredCommand)
  hFlush stdout
  getInput' inferredCommand Normal

data WasAutoCompleteMatchFound = NoAutoCompleteMatchFound | AutoCompleteMatchFound String | AutoCompleteMatchesFound [String] deriving (Show)

findBuiltInAutoCompleteMatch :: String -> WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatch partialCommand = findBuiltInAutoCompleteMatch' partialCommand ["exit", "echo"]

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
  pure $ findBuiltInAutoCompleteMatch' partialCommand allExecutables

findBuiltInAutoCompleteMatch' :: String -> [String] -> WasAutoCompleteMatchFound
findBuiltInAutoCompleteMatch' partialCommand builtins =
  let filteredBuiltins = filter (doesPartialCommandMatchBuiltin partialCommand) builtins
   in case filteredBuiltins of
        [] -> NoAutoCompleteMatchFound
        [x] -> AutoCompleteMatchFound ((head filteredBuiltins) ++ " ")
        _ -> AutoCompleteMatchesFound filteredBuiltins

doesPartialCommandMatchBuiltin :: String -> String -> Bool
doesPartialCommandMatchBuiltin partialCommand builtin = partialCommand /= "" && partialCommand `isPrefixOf` builtin

main :: IO ()
main = do
  initGetCharNoBuffering
  hSetEcho stdin False
  main'

main' :: IO ()
main' = do
  putStr "$ "
  hFlush stdout
  args <- getInput
  evaluatedResult <- eval args
  handleEval evaluatedResult

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
  PrintStdOutAndContinue stdOut -> printAndContinue stdOut
  PrintStdErrAndContinue stdErr -> printAndContinue stdErr
  PrintStdOutAndPrintStdErrAndContinue stdOut stdErr -> printAndContinue stdErr
  RedirectStdOutAndPrintStdErrAndContinue stdOut file stdErr redirectMode -> redirectStdOutAndPrintStdErrAndContinue stdOut file stdErr redirectMode
  PrintStdOutAndRedirectStdErrAndContinue stdOut file stdErr redirectMode -> redirectStdOutAndPrintStdErrAndContinue stdErr file stdOut redirectMode
  RedirectStdOutAndContinue stdOut file redirectMode -> redirectStdOutAndContinue stdOut file redirectMode
  RedirectStdErrAndContinue stdErr file redirectMode -> redirectStdOutAndContinue stdErr file redirectMode
  Continue -> main'
  Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
  printStrIfNonEmpty str
  hFlush stdout
  main'

printStrIfNonEmpty :: String -> IO ()
printStrIfNonEmpty "" = pure ()
printStrIfNonEmpty str = do
  putStrLn str
  hFlush stdout

writeOrAppendFile :: String -> String -> RedirectMode -> IO ()
writeOrAppendFile str file redirectMode = case redirectMode of
  Overwrite -> do
    writeFile file str
  Append -> do
    fileExists <- doesFileExist file
    if fileExists then handleAppendToFileThatExists str file else appendFile file str

handleAppendToFileThatExists :: String -> String -> IO ()
handleAppendToFileThatExists str file = do
  lineCount <- countLines file
  if lineCount == 0 then appendFile file str else appendFile file ("\n" ++ str)

countLines :: FilePath -> IO Int
countLines path = do
  contents <- readFile path
  return (length (lines contents))

redirectStdOutAndContinue :: String -> String -> RedirectMode -> IO ()
redirectStdOutAndContinue stdOut file redirectMode = do
  writeOrAppendFile stdOut file redirectMode
  main'

redirectStdOutAndPrintStdErrAndContinue :: String -> String -> String -> RedirectMode -> IO ()
redirectStdOutAndPrintStdErrAndContinue stdOut file stdErr redirectMode = do
  writeOrAppendFile stdOut file redirectMode
  printStrIfNonEmpty stdErr
  main'

eval :: String -> IO EvaluatedResult
eval untokenizedArgs = if null untokenizedArgs then pure Continue else modifyEvaluatedResultWithRedirectFile (eval' command args) redirectStdToFile
  where
    tokenizedArgs = tokenize untokenizedArgs
    command = head tokenizedArgs
    (args, redirectStdToFile) = getArgsAndRedirectStdToFile (tail tokenizedArgs)

modifyEvaluatedResultWithRedirectFile :: IO EvaluatedResult -> RedirectStdToFile -> IO EvaluatedResult
modifyEvaluatedResultWithRedirectFile ioEvaluatedResult redirectStdToFile = do
  evaluatedResult <- ioEvaluatedResult
  case redirectStdToFile of
    NoRedirect -> pure evaluatedResult
    RedirectStdErrToFile redirectMode file -> case evaluatedResult of
      (PrintStdOutAndContinue str) -> pure (PrintStdOutAndRedirectStdErrAndContinue str file "" redirectMode)
      (PrintStdErrAndContinue str) -> pure (RedirectStdErrAndContinue str file redirectMode)
      (PrintStdOutAndPrintStdErrAndContinue stdOut stdErr) -> pure (PrintStdOutAndRedirectStdErrAndContinue stdOut file stdErr redirectMode)
      _ -> pure evaluatedResult
    RedirectStdOutToFile redirectMode file -> case evaluatedResult of
      (PrintStdOutAndContinue str) -> pure (RedirectStdOutAndContinue str file redirectMode)
      (PrintStdOutAndPrintStdErrAndContinue stdOut stdErr) -> pure (RedirectStdOutAndPrintStdErrAndContinue stdOut file stdErr redirectMode)
      _ -> pure evaluatedResult

getArgsAndRedirectStdToFile :: [String] -> ([String], RedirectStdToFile)
getArgsAndRedirectStdToFile tokenizedArgs =
  let args = takeWhile tokenIsNotRedirectOperator tokenizedArgs
      file = getRedirectFile args tokenizedArgs
      redirectStdToFile = getRedirectStdToFile tokenizedArgs file
   in (args, redirectStdToFile)

getRedirectMode :: [String] -> RedirectMode
getRedirectMode args = if (">>") `elem` args || ("1>>") `elem` args || ("2>>") `elem` args then Append else Overwrite

getRedirectStdToFile :: [String] -> Maybe String -> RedirectStdToFile
getRedirectStdToFile args Nothing = NoRedirect
getRedirectStdToFile args (Just file) = if hasStdErrRedirectOperator args then RedirectStdErrToFile redirectMode file else RedirectStdOutToFile redirectMode file
  where
    redirectMode = getRedirectMode args

getRedirectFile :: [String] -> [String] -> Maybe String
getRedirectFile args tokenizedArgs =
  if (length args == length tokenizedArgs || null tokenizedArgs)
    then Nothing
    else case drop 1 (dropWhile tokenIsNotRedirectOperator tokenizedArgs) of
      [] -> Nothing
      val -> Just (last val)

hasStdErrRedirectOperator :: [String] -> Bool
hasStdErrRedirectOperator args = "2>" `elem` args || "2>>" `elem` args

tokenIsNotRedirectOperator :: String -> Bool
tokenIsNotRedirectOperator token = token /= ">" && token /= "1>" && token /= "2>" && token /= ">>" && token /= "1>>" && token /= "2>>"

eval' :: String -> [String] -> IO EvaluatedResult
eval' command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintStdOutAndContinue (unwords args)
  "pwd" -> PrintStdOutAndContinue <$> getCurrentDirectory
  "cd" -> handleChangeDirectoryCommand (unwords args)
  "type" -> handleTypeCommand (unwords args)
  _ -> handleUnknownCommand command args

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
        ExitFailure _ -> pure (PrintStdOutAndPrintStdErrAndContinue (removeLastNewline stdOut) (removeLastNewline err))

handleChangeDirectoryCommand :: String -> IO EvaluatedResult
handleChangeDirectoryCommand path = do
  homeDir <- getHomeDirectory
  let parsedPath = replaceString "~" homeDir path
  directoryExists <- doesDirectoryExist parsedPath
  curDir <- getCurrentDirectory
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
