-- ghc --make WordCount.hs
module Main where

import System.Environment


main :: IO ()
main = do
  args <- getArgs
  print args
  fileContents <- mapM readFile args
  -- mapM_ putStr fileContents
  fileCounts <- mapM wcCount fileContents
  putStrLn "hi"

  


  --or, to get "UNIX cat":
  --fileContents <- mapM readFile args
  --mapM_ putStr fileContents

wcCount :: String -> [Int]
wcCount str = [length (lines str), length (words str), length str]
