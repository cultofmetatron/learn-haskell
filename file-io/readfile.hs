import System.IO

main :: IO()
main = do
  handle <- openFile "hello.hs" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
