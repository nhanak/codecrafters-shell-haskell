module ShellState.Core (ShellState (..), CompleterScript (..), initialShellState, getCompleterScript') where

data ShellState = ShellState {backgroundJobId :: Int, completerScripts :: [CompleterScript], history :: [String]} deriving (Show)

data CompleterScript = CompleterScript {path :: String, command :: String} deriving (Show)

initialShellState :: ShellState
initialShellState = ShellState {completerScripts = [], history = [], backgroundJobId = 1}

getCompleterScript' :: String -> [CompleterScript] -> Maybe CompleterScript
getCompleterScript' command_ completerScripts = case filter (\x -> command x == command_) completerScripts of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing
