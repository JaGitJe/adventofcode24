module Day02 (getReports, countSafe) where

testInput :: String
testInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

getReports :: String -> [[Int]]
getReports = map (map read . words) . lines

adj :: Int -> Int -> Bool
adj a b = let d = abs (a - b) in d >= 1 && d <= 3

safe :: [Int] -> Bool
safe (x : y : xs)
  | x > y = chg (>) x (y : xs)
  | x < y = chg (<) x (y : xs)
  | otherwise = False
 where
  chg _ _ [] = True
  chg f prev (x' : xs') =
    (f prev x' && adj prev x') && chg f x' xs'
safe _ = False

countSafe :: [[Int]] -> Int
countSafe = length . filter safe
