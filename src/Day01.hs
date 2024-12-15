module Day01 (sumDst, sumSimScore, getSortedPairs, getLists) where

import Data.List (sort)

getLists :: String -> ([Int], [Int])
getLists s = unzip $ map ((\[a, b] -> (read a, read b)) . words) . lines $ s

getSortedPairs :: ([Int], [Int]) -> [(Int, Int)]
getSortedPairs = uncurry zip . (\(l, r) -> (sort l, sort r))

simScore :: Int -> [Int] -> Int
simScore n = sum . filter (n ==)

sumSimScore :: ([Int], [Int]) -> Int
sumSimScore (l, r) = (sum . map (`simScore` r)) l

sumDst :: [(Int, Int)] -> Int
sumDst = foldl (\s (a, b) -> s + abs (a - b)) 0
