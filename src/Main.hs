module Main where

import Day01
import Day02 (countSafe, getReports)

runDay :: (String, String -> Int) -> IO ()
runDay (path, f) = do
  udata <- readFile path
  let result = f udata
  print result

d01p1 :: (String, String -> Int)
d01p1 = ("data/d01p1.txt", sumDst . getSortedPairs . getLists)

d01p2 :: (String, String -> Int)
d01p2 = ("data/d01p2.txt", sumSimScore . getLists)

d02p1 :: (String, String -> Int)
d02p1 = ("data/d02p1.txt", countSafe . getReports)

main :: IO ()
main = putStrLn "Hello, Haskell!"
