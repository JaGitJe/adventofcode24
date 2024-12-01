module Main where

import Day01

runDay :: (String, String -> Integer) -> IO ()
runDay (path, f) = do
  udata <- readFile path
  let result = f udata
  print result

d01p1 :: (String, String -> Integer)
d01p1 = ("data/d01p1.txt", sumDst . getPairs . getSortedLists)

main :: IO ()
main = putStrLn "Hello, Haskell!"
