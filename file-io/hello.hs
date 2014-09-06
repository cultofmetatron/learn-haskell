module Main where
import System.Environment


main :: IO()
main = do
  args  <- getLine
  args2 <- getLine
  putStrLn ("Hello " ++ args ++ args2)
