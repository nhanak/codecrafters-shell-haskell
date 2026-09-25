module Core (getArgsWithoutProcessPrioritySignifier, getProcessPriority, ProcessPriority (..)) where

data ProcessPriority = Foreground | Background deriving (Show, Eq)

getProcessPriority :: [String] -> ProcessPriority
getProcessPriority args = if last args == "&" then Background else Foreground

getArgsWithoutProcessPrioritySignifier :: [String] -> [String]
getArgsWithoutProcessPrioritySignifier args = if (getProcessPriority args) == Foreground then args else init args
