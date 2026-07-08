module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "$ "
  hFlush stdout
  word <- getLine
  putStrLn $ word <> ": command not found"
  hFlush stdout
  pure ()
