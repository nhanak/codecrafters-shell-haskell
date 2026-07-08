module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  command <- read'
  print' $ eval command
  hFlush stdout
  main

read' :: IO String
read' = getLine

eval :: String -> String
eval command = command <> ": command not found"

print' :: String -> IO ()
print' = putStrLn
