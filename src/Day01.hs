{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day01 (sumDst, getPairs, getSortedLists) where

import Data.List

testInput :: String
testInput = "14   12\n4   3\n2   2\n1   2\n7   9\n0   5"

getSortedLists :: String -> ([Integer], [Integer])
getSortedLists s = (sort l, sort r)
 where
  (l, r) = unzip $ map ((\[a, b] -> (read a, read b)) . words) . lines $ s

getPairs :: ([Integer], [Integer]) -> [(Integer, Integer)]
getPairs = uncurry zip

sumDst :: [(Integer, Integer)] -> Integer
sumDst = foldl (\s (a, b) -> s + abs (a - b)) 0
