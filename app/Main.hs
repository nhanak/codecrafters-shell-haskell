module Main (main) where

import Control.Concurrent (MVar (..), forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.State
import Core (ProcessPriority (..), getArgsWithoutProcessPrioritySignifier, getProcessPriority)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import Input (getInput)
import ShellState.Core (CompleterScript (..), ShellState (..), formatBackgroundJobsForPrinting, initialShellState)
import ShellState.IO (getBackgroundJobs, getCompleterScript, getNextBackgroundJobId, io, registerBackgroundJob, registerCompleterScript, removeCompleterScript)
import System.Directory (Permissions, doesDirectoryExist, doesFileExist, executable, findExecutable, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (getSearchPath, pathSeparator, takeBaseName, takeFileName)
import System.IO (hFlush, hSetEcho, stdin, stdout)
import System.IO.NoBufferingWorkaround (getCharNoBuffering, initGetCharNoBuffering)
import System.Process (createProcess, getPid, proc, readProcessWithExitCode)
import Tokenizer (tokenize)

data EvaluatedResult = PrintStdOutAndContinue String | PrintStdErrAndContinue String | Exit | Continue | RedirectStdOutAndContinue String String RedirectMode | RedirectStdErrAndContinue String String RedirectMode | PrintStdOutAndRedirectStdErrAndContinue String String String RedirectMode | RedirectStdOutAndPrintStdErrAndContinue String String String RedirectMode | PrintStdOutAndPrintStdErrAndContinue String String deriving (Show)

data RedirectStdToFile = RedirectStdOutToFile RedirectMode String | RedirectStdErrToFile RedirectMode String | NoRedirect deriving (Show)

data RedirectMode = Append | Overwrite deriving (Show)

main :: IO ()
main = do
  initGetCharNoBuffering
  hSetEcho stdin False
  runStateT main' initialShellState >> pure ()

main' :: StateT ShellState IO ()
main' = do
  io $ putStr "$ "
  io $ hFlush stdout
  args <- getInput
  evaluatedResult <- eval args
  handleEval evaluatedResult

handleEval :: EvaluatedResult -> StateT ShellState IO ()
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

printAndContinue :: String -> StateT ShellState IO ()
printAndContinue str = do
  io $ printStrIfNonEmpty str
  io $ hFlush stdout
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

redirectStdOutAndContinue :: String -> String -> RedirectMode -> StateT ShellState IO ()
redirectStdOutAndContinue stdOut file redirectMode = do
  io $ writeOrAppendFile stdOut file redirectMode
  main'

redirectStdOutAndPrintStdErrAndContinue :: String -> String -> String -> RedirectMode -> StateT ShellState IO ()
redirectStdOutAndPrintStdErrAndContinue stdOut file stdErr redirectMode = do
  io $ writeOrAppendFile stdOut file redirectMode
  io $ printStrIfNonEmpty stdErr
  main'

eval :: String -> StateT ShellState IO EvaluatedResult
eval untokenizedArgs = if null untokenizedArgs then pure Continue else modifyEvaluatedResultWithRedirectFile (eval' command args processPriority) redirectStdToFile
  where
    tokenizedArgs = tokenize untokenizedArgs
    command = head tokenizedArgs
    (argsRaw, redirectStdToFile) = getArgsAndRedirectStdToFile (tail tokenizedArgs)
    processPriority = getProcessPriority argsRaw
    args = getArgsWithoutProcessPrioritySignifier argsRaw

modifyEvaluatedResultWithRedirectFile :: StateT ShellState IO EvaluatedResult -> RedirectStdToFile -> StateT ShellState IO EvaluatedResult
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

handleEvalForeground :: String -> [String] -> StateT ShellState IO EvaluatedResult
handleEvalForeground command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintStdOutAndContinue (unwords args)
  "pwd" -> io $ (PrintStdOutAndContinue <$> getCurrentDirectory)
  "cd" -> io $ handleChangeDirectoryCommand (unwords args)
  "type" -> io $ handleTypeCommand (unwords args)
  "complete" -> handleCompleteCommand args
  "jobs" -> handleJobsCommand args
  _ -> io $ handleUnknownCommand command args

eval' :: String -> [String] -> ProcessPriority -> StateT ShellState IO EvaluatedResult
eval' command args processPriority = case processPriority of
  Foreground -> handleEvalForeground command args
  Background -> handleEvalBackground command args

handleEvalBackground :: String -> [String] -> StateT ShellState IO EvaluatedResult
handleEvalBackground command args = case command of
  "exit" -> pure Exit
  "echo" -> pure $ PrintStdOutAndContinue (unwords args)
  "pwd" -> io $ (PrintStdOutAndContinue <$> getCurrentDirectory)
  "cd" -> io $ handleChangeDirectoryCommand (unwords args)
  "type" -> io $ handleTypeCommand (unwords args)
  "complete" -> handleCompleteCommand args
  "jobs" -> handleJobsCommand args
  _ -> handleUnknownCommandBackground command args

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

handleUnknownCommandBackground :: String -> [String] -> StateT ShellState IO EvaluatedResult
handleUnknownCommandBackground command args = do
  str <- io $ _findExecutable command
  if "not found" `isInfixOf` str
    then pure $ PrintStdErrAndContinue str
    else do
      maybePidMVar <- io $ newEmptyMVar
      _ <- io $ forkIO $ do
        (stdInHandle, stdOutHandle, stdErrhandle, processHandle) <- createProcess (proc (takeFileName str) args)
        maybePid <- getPid processHandle
        putMVar maybePidMVar maybePid
      maybePid <- io $ takeMVar maybePidMVar
      case maybePid of
        Nothing -> pure Continue
        Just pid -> do
          backgroundId <- getNextBackgroundJobId
          registerBackgroundJob backgroundId (fromIntegral pid) (unwords ([command] ++ args ++ ["&"]))
          pure $ PrintStdOutAndContinue ("[" ++ show backgroundId ++ "] " ++ show pid)

-- evaluatedResult <- io $ takeMVar evaluatedResultMvar
-- evaluatedResult

-- handleUnknownCommandBackground :: String -> [String] -> StateT ShellState IO EvaluatedResult
-- handleUnknownCommandBackground command args = do
--   str <- io $ _findExecutable command
--   if "not found" `isInfixOf` str
--     then pure $ PrintStdErrAndContinue str
--     else do
--       evaluatedResultMvar <- io $ newEmptyMVar
--       backgroundId <- getNextBackgroundJobId
--       _ <- io $ forkIO $ do
--         (stdInHandle, stdOutHandle, stdErrhandle, processHandle) <- createProcess (proc (takeFileName str) args)
--         maybePid <- getPid processHandle
--         case maybePid of
--           Just pid -> do
--             registerBackgroundJob backgroundId pid (unwords ([command] ++ args))
--             putMVar evaluatedResultMvar (io $ pure (PrintStdOutAndContinue ("[" ++ show backgroundId ++ "] " ++ show pid)))
--           Nothing -> putMVar evaluatedResultMvar (io $ pure Continue)
--       evaluatedResult <- io $ takeMVar evaluatedResultMvar
--       evaluatedResult
--

handleJobsCommand :: [String] -> StateT ShellState IO EvaluatedResult
handleJobsCommand args = do
  backgroundJobs <- getBackgroundJobs
  pure $ PrintStdOutAndContinue $ drop 1 $ concat $ formatBackgroundJobsForPrinting backgroundJobs

handleCompleteCommand :: [String] -> StateT ShellState IO EvaluatedResult
handleCompleteCommand args = case args of
  ["-C"] -> pure $ PrintStdOutAndContinue ("complete: -C flag used but no no completion specification")
  ("-C" : path : command : rest) -> do
    registerCompleterScript path command
    pure $ Continue
  ("-r" : command : xs) -> do
    removeCompleterScript command
    pure $ Continue
  ("-p" : command : xs) -> do
    completerScript <- getCompleterScript command
    case completerScript of
      Nothing -> pure $ PrintStdOutAndContinue ("complete: " ++ command ++ ": no completion specification")
      Just script -> pure $ PrintStdOutAndContinue ("complete -C \'" ++ (path script) ++ "\' " ++ command)
  _ -> pure $ PrintStdOutAndContinue ("incorrect usage of command complete")

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
  x | x `elem` ["exit", "echo", "type", "pwd", "cd", "complete", "jobs"] -> pure $ PrintStdOutAndContinue (x <> " is a shell builtin")
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
