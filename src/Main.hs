module Main where

import Day01
import Day02
import Day03

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
d02p1 = ("data/d02p1.txt", countSafe safe . getReports)

d02p2 :: (String, String -> Int)
d02p2 = ("data/d02p1.txt", countSafe damp . getReports)

d03p1 :: (String, String -> Int)
d03p1 = ("data/d03p1.txt", mul . parseMem)

main :: IO ()
main = putStrLn "Hello, Haskell!"
