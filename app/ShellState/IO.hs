module ShellState.IO (io, getCompleterScript, getCompleterScriptCommands, getNextBackgroundJobId, removeCompleterScript, registerCompleterScript) where

import Control.Monad.State
import ShellState.Core (CompleterScript (..), ShellState (..), getCompleterScript')

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
  put $ prevState {backgroundJobId = backgroundJobId prevState + 1}
  pure $ backgroundJobId prevState

removeCompleterScript :: String -> StateT ShellState IO ()
removeCompleterScript command_ = do
  prevState <- get
  put $ prevState {completerScripts = filter (\completerScript -> command_ /= command completerScript) (completerScripts prevState)}

registerCompleterScript :: String -> String -> StateT ShellState IO ()
registerCompleterScript path command = do
  prevState <- get
  put $ prevState {completerScripts = (completerScripts prevState) ++ [CompleterScript {path = path}]}
