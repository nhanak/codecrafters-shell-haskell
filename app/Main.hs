module Main (main) where

import System.IO (hFlush, stdout)

data EvaluatedResult = Print String | Exit | Empty

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  args <- read'
  handleEval $ eval args

read' :: IO String
read' = getLine

eval :: String -> EvaluatedResult
eval args = if null args then Empty else eval' (getCommand args) (getRemainingArgs args)

eval' :: String -> String -> EvaluatedResult
eval' command remainingArgs = case command of
  "exit" -> Exit
  "echo" -> Print $ remainingArgs
  _ -> Print $ command <> ": command not found"

getCommand :: String -> String
getCommand args = head (words args)

getRemainingArgs :: String -> String
getRemainingArgs args = unwords (tail $ words args)

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
  Print str -> printAndContinue str
  Empty -> main
  Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
  putStrLn str
  hFlush stdout
  main
