module Main (main) where

import System.IO (hFlush, stdout)

data EvaluatedResult = Print String | Exit

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  command <- read'
  handleEval $ eval command

read' :: IO String
read' = getLine

eval :: String -> EvaluatedResult
eval command = case command of
  "exit" -> Exit
  otherwise -> Print $ command <> ": command not found"

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
  Print str -> printAndContinue str
  Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
  putStrLn str
  hFlush stdout
  main
