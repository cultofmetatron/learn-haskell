module Main where
import System.Environment

caller :: IO String
caller = do
  line1 <- getLine
  return line1

main :: IO()
main = do
  line <- caller
  if line == "quit"
    then do
      return ()
    else do
      putStrLn line
      main

