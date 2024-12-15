module Main where

import Day01
import Day02
import Day03
import Day05
import Day06
import Day07
import Day10

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

d05p1 :: (String, String -> Int)
d05p1 = ("data/d05p1.txt", sumMidPages)

d06p1 :: (String, String -> Int)
d06p1 = ("data/d06p1.txt", patrolPositions . getFloorplan)

d07p1 :: (String, String -> Int)
d07p1 = ("data/d07p1.txt", sumSolvable . getEquations)

d10p1 :: (String, String -> Int)
d10p1 = ("data/d10p1.txt", countTrails . getMap)

main :: IO ()
main = putStrLn "Hello, Haskell!"
