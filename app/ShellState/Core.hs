module ShellState.Core (getMostRecentBackgroundJobPid, getSecondMostRecentBackgroundJobPid, getFormattedBackgroundJobsString, ShellState (..), CompleterScript (..), initialShellState, getCompleterScript', BackgroundJobStatus (..), BackgroundJob (..)) where

import System.Process (ProcessHandle)

data ShellState = ShellState {backgroundJobs :: [BackgroundJob], currentBackgroundJobId :: Int, completerScripts :: [CompleterScript], history :: [String]}

data CompleterScript = CompleterScript {path :: String, command :: String} deriving (Show)

data BackgroundJob = BackgroundJob {backgroundJobProcessHandle :: ProcessHandle, backgroundJobPid :: Int, backgroundJobId :: Int, backgroundJobStatus :: BackgroundJobStatus, backgroundJobCommand :: String}

data BackgroundJobStatus = Running | Done deriving (Show, Eq)

initialShellState :: ShellState
initialShellState = ShellState {completerScripts = [], history = [], currentBackgroundJobId = 1, backgroundJobs = []}

getCompleterScript' :: String -> [CompleterScript] -> Maybe CompleterScript
getCompleterScript' command_ completerScripts = case filter (\x -> command x == command_) completerScripts of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing

getFormattedBackgroundJobsString :: [BackgroundJob] -> [BackgroundJob] -> String
getFormattedBackgroundJobsString allBackgroundJobs backgroundJobsToBeFormatted = init $ concat $ (map (formatBackgroundJobForPrinting (getMostRecentBackgroundJobPid allBackgroundJobs) (getSecondMostRecentBackgroundJobPid allBackgroundJobs) (length allBackgroundJobs)) backgroundJobsToBeFormatted)

formatBackgroundJobsForPrinting :: [BackgroundJob] -> [String]
formatBackgroundJobsForPrinting backgroundJobs = if null backgroundJobs then [] else map (formatBackgroundJobForPrinting (getMostRecentBackgroundJobPid backgroundJobs) (getSecondMostRecentBackgroundJobPid backgroundJobs) (length backgroundJobs)) backgroundJobs

formatBackgroundJobForPrinting :: Int -> Int -> Int -> BackgroundJob -> String
formatBackgroundJobForPrinting mostRecentPid secondMostRecentPid numJobs backgroundJob = idStr ++ marker ++ "  " ++ status ++ spacesAfterStatus ++ backgroundJobCommand backgroundJob ++ newLine
  where
    idStr = "[" ++ show (backgroundJobId backgroundJob) ++ "]"
    marker = getBackgroundJobMarker mostRecentPid secondMostRecentPid (backgroundJobPid backgroundJob)
    status = show $ backgroundJobStatus backgroundJob
    spacesAfterStatus = concat $ replicate (24 - length status) " "
    newLine = if numJobs > 1 then "\n" else ""

getMostRecentBackgroundJobPid :: [BackgroundJob] -> Int
getMostRecentBackgroundJobPid backgroundJobs = backgroundJobPid $ last backgroundJobs

getSecondMostRecentBackgroundJobPid :: [BackgroundJob] -> Int
getSecondMostRecentBackgroundJobPid backgroundJobs = if length backgroundJobs < 2 then (-1) else backgroundJobPid $ last $ init backgroundJobs

getBackgroundJobMarker :: Int -> Int -> Int -> String
getBackgroundJobMarker mostRecentPid secondMostRecentPid backgroundJobPid
  | mostRecentPid == backgroundJobPid = "+"
  | secondMostRecentPid == backgroundJobPid = "-"
  | otherwise = " "
