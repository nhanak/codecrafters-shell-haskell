module ShellState.IO (io, getCompleterScript, getCompleterScriptCommands, getNextBackgroundJobId, removeCompleterScript, registerCompleterScript, registerBackgroundJob, getBackgroundJobs, markDoneBackgroundJobs, reapDoneBackgroundJobs) where

import Control.Monad.State
import ShellState.Core (BackgroundJob (..), BackgroundJobStatus (..), CompleterScript (..), ShellState (..), getCompleterScript')
import System.Process (ProcessHandle, getProcessExitCode)

io :: IO a -> StateT ShellState IO a
io = liftIO

getCompleterScript :: String -> StateT ShellState IO (Maybe CompleterScript)
getCompleterScript command_ = do
  curState <- get
  pure $ getCompleterScript' command_ (completerScripts curState)

getCompleterScriptCommands :: StateT ShellState IO [String]
getCompleterScriptCommands = do
  curState <- get
  pure $ map command (completerScripts curState)

getNextBackgroundJobId :: StateT ShellState IO Int
getNextBackgroundJobId = do
  prevState <- get
  put $ prevState {currentBackgroundJobId = currentBackgroundJobId prevState + 1}
  pure $ currentBackgroundJobId prevState

removeCompleterScript :: String -> StateT ShellState IO ()
removeCompleterScript command_ = do
  prevState <- get
  put $ prevState {completerScripts = filter (\completerScript -> command_ /= command completerScript) (completerScripts prevState)}

registerCompleterScript :: String -> String -> StateT ShellState IO ()
registerCompleterScript path command = do
  prevState <- get
  put $ prevState {completerScripts = (completerScripts prevState) ++ [CompleterScript {path = path, command = command}]}

registerBackgroundJob :: Int -> Int -> String -> ProcessHandle -> StateT ShellState IO ()
registerBackgroundJob jobId pid command processHandle = do
  prevState <- get
  put $ prevState {backgroundJobs = backgroundJobs prevState ++ [BackgroundJob {backgroundJobProcessHandle = processHandle, backgroundJobCommand = command, backgroundJobId = jobId, backgroundJobPid = pid, backgroundJobStatus = Running}]}

getBackgroundJobs :: StateT ShellState IO [BackgroundJob]
getBackgroundJobs = do
  curState <- get
  pure $ backgroundJobs curState

markDoneBackgroundJobs :: StateT ShellState IO ()
markDoneBackgroundJobs = do
  prevState <- get
  nextBackgroundJobs <- io $ mapM markDoneBackgroundJob (backgroundJobs prevState)
  put $ prevState {backgroundJobs = nextBackgroundJobs}
  pure ()

markDoneBackgroundJob :: BackgroundJob -> IO BackgroundJob
markDoneBackgroundJob backgroundJob = do
  maybeExitCode <- getProcessExitCode (backgroundJobProcessHandle backgroundJob)
  case maybeExitCode of
    Nothing -> pure $ backgroundJob
    Just x -> pure $ backgroundJob {backgroundJobStatus = Done, backgroundJobCommand = (init $ init $ backgroundJobCommand backgroundJob)}

reapDoneBackgroundJobs :: StateT ShellState IO ()
reapDoneBackgroundJobs = do
  prevState <- get
  put $ prevState {backgroundJobs = (filter (\backgroundJob -> backgroundJobStatus backgroundJob /= Done) (backgroundJobs prevState))}
  pure ()
