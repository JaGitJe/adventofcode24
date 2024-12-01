{-# HLINT ignore "Use bimap" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day01 (sumDst, sumSimScore, getSortedPairs, getLists) where

import Data.List (sort)

testInput :: String
testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

getLists :: String -> ([Integer], [Integer])
getLists s = unzip $ map ((\[a, b] -> (read a, read b)) . words) . lines $ s

getSortedPairs :: ([Integer], [Integer]) -> [(Integer, Integer)]
getSortedPairs = uncurry zip . (\(l, r) -> (sort l, sort r))

simScore :: Integer -> [Integer] -> Integer
simScore n = sum . filter (n ==)

sumSimScore :: ([Integer], [Integer]) -> Integer
sumSimScore (l, r) = (sum . map (`simScore` r)) l

sumDst :: [(Integer, Integer)] -> Integer
sumDst = foldl (\s (a, b) -> s + abs (a - b)) 0
