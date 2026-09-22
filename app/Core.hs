module Core (io, ShellState (..), CompleterScript (..), getCompleterScript, getCompleterScriptCommands) where

import Control.Monad.State

data ShellState = ShellState {completerScripts :: [CompleterScript], history :: [String]} deriving (Show)

data CompleterScript = CompleterScript {path :: String, command :: String} deriving (Show)

io :: IO a -> StateT ShellState IO a
io = liftIO

getCompleterScript :: String -> StateT ShellState IO (Maybe CompleterScript)
getCompleterScript command_ = do
  curState <- get
  pure $ getCompleterScript' command_ (completerScripts curState)

getCompleterScript' :: String -> [CompleterScript] -> Maybe CompleterScript
getCompleterScript' command_ completerScripts = case filter (\x -> command x == command_) completerScripts of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing

getCompleterScriptCommands :: StateT ShellState IO [String]
getCompleterScriptCommands = do
  curState <- get
  pure $ map command (completerScripts curState)
