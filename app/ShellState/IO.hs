module ShellState.IO (unregisterPipelineProcessesIfDone, io, getCompleterScript, getCompleterScriptCommands, getNextBackgroundJobId, removeCompleterScript, getDoneBackgroundJobs, registerPipelineProcesses, getPipelineProcesses, clearPipelineProcesses, isPipeProcessRunning, registerCompleterScript, registerBackgroundJob, getBackgroundJobs, markDoneBackgroundJobs, reapDoneBackgroundJobs) where

import Control.Monad (mapM, when)
import Control.Monad.State
import Data.Foldable (traverse_)
import ShellState.Core (BackgroundJob (..), BackgroundJobStatus (..), CompleterScript (..), PipelineProcess, ShellState (..), getCompleterScript', getDoneBackgroundJobIds)
import System.Process (ProcessHandle, cleanupProcess, getProcessExitCode)

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
  if null $ reusableBackgroundJobIds prevState
    then do
      put $ prevState {currentBackgroundJobId = currentBackgroundJobId prevState + 1}
      pure $ currentBackgroundJobId prevState
    else do
      put $ prevState {reusableBackgroundJobIds = tail (reusableBackgroundJobIds prevState)}
      pure $ head (reusableBackgroundJobIds prevState)

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

unregisterPipelineProcessesIfDone :: StateT ShellState IO ()
unregisterPipelineProcessesIfDone = do
  prevState <- get
  areAllPipelineProcessesAreDone <- io $ allPipelineProcessesAreDone prevState
  -- io $ putStrLn ("[DEBUG]: unregisterPipelineProcessesIfDone")
  -- io $ putStrLn ("[DEBUG]: allProcessesAreDone: " ++ show areAllPipelineProcessesAreDone)
  when areAllPipelineProcessesAreDone $
    do
      io $ cleanupOrphanPipelineProcesses (pipelineProcesses prevState)
      put $ prevState {pipelineProcesses = []}

cleanupOrphanPipelineProcesses :: [PipelineProcess] -> IO ()
cleanupOrphanPipelineProcesses processes = traverse_ cleanupProcess processes

allPipelineProcessesAreDone :: ShellState -> IO Bool
allPipelineProcessesAreDone shellState = do
  arePipelineProcessesDone <- mapM isPipelineProcessDone (pipelineProcesses shellState)
  -- putStrLn ("[DEBUG]: allProcessesAreDone inner: " ++ show arePipelineProcessesDone)
  pure $ True `elem` arePipelineProcessesDone

isPipelineProcessDone :: PipelineProcess -> IO Bool
isPipelineProcessDone (_, _, _, ph) = do
  exitCode <- getProcessExitCode ph
  if null exitCode then pure False else pure True

registerPipelineProcesses :: [PipelineProcess] -> StateT ShellState IO ()
registerPipelineProcesses pipelineProcesses = do
  -- io $ putStrLn ("[DEBUG]: registerPipelineProcesses")
  prevState <- get
  put $ prevState {pipelineProcesses = pipelineProcesses}

getPipelineProcesses :: StateT ShellState IO [PipelineProcess]
getPipelineProcesses = do
  prevState <- get
  pure $ pipelineProcesses prevState

clearPipelineProcesses :: StateT ShellState IO ()
clearPipelineProcesses = do
  prevState <- get
  put $ prevState {pipelineProcesses = []}

isPipeProcessRunning :: StateT ShellState IO Bool
isPipeProcessRunning = do
  curState <- get
  pure $ not (null $ pipelineProcesses curState)

getBackgroundJobs :: StateT ShellState IO [BackgroundJob]
getBackgroundJobs = do
  curState <- get
  pure $ backgroundJobs curState

getDoneBackgroundJobs :: StateT ShellState IO [BackgroundJob]
getDoneBackgroundJobs = do
  curState <- get
  pure $ filter (\backgroundJob -> backgroundJobStatus backgroundJob == Done) (backgroundJobs curState)

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
    Just x -> pure $ backgroundJob {backgroundJobStatus = Done, backgroundJobCommand = (init $ backgroundJobCommand backgroundJob)}

reapDoneBackgroundJobs :: StateT ShellState IO ()
reapDoneBackgroundJobs = do
  prevState <- get
  put $ prevState {reusableBackgroundJobIds = reusableBackgroundJobIds prevState ++ getDoneBackgroundJobIds (backgroundJobs prevState), backgroundJobs = (filter (\backgroundJob -> backgroundJobStatus backgroundJob /= Done) (backgroundJobs prevState))}
  pure ()
