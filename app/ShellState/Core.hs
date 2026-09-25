module ShellState.Core (formatBackgroundJobsForPrinting, ShellState (..), CompleterScript (..), initialShellState, getCompleterScript', BackgroundJobStatus (..), BackgroundJob (..)) where

data ShellState = ShellState {backgroundJobs :: [BackgroundJob], currentBackgroundJobId :: Int, completerScripts :: [CompleterScript], history :: [String]} deriving (Show)

data CompleterScript = CompleterScript {path :: String, command :: String} deriving (Show)

data BackgroundJob = BackgroundJob {backgroundJobPid :: Int, backgroundJobId :: Int, backgroundJobStatus :: BackgroundJobStatus, backgroundJobCommand :: String} deriving (Show)

data BackgroundJobStatus = Running | Finished deriving (Show)

initialShellState :: ShellState
initialShellState = ShellState {completerScripts = [], history = [], currentBackgroundJobId = 1, backgroundJobs = []}

getCompleterScript' :: String -> [CompleterScript] -> Maybe CompleterScript
getCompleterScript' command_ completerScripts = case filter (\x -> command x == command_) completerScripts of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing

formatBackgroundJobsForPrinting :: [BackgroundJob] -> [String]
formatBackgroundJobsForPrinting backgroundJobs = if null backgroundJobs then [] else map (formatBackgroundJobForPrinting (backgroundJobPid (last backgroundJobs)) (length backgroundJobs)) backgroundJobs

formatBackgroundJobForPrinting :: Int -> Int -> BackgroundJob -> String
formatBackgroundJobForPrinting mostRecentPid numJobs backgroundJob = idStr ++ marker ++ "  " ++ status ++ spacesAfterStatus ++ backgroundJobCommand backgroundJob ++ newLine
  where
    idStr = "[" ++ show (backgroundJobId backgroundJob) ++ "]"
    marker = if mostRecentPid == backgroundJobPid backgroundJob then "+" else ""
    status = show $ backgroundJobStatus backgroundJob
    spacesAfterStatus = concat $ replicate (24 - length status) " "
    newLine = if numJobs > 1 then "\n" else ""
