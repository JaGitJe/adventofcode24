module Day02 (getReports, countSafe, safe, damp) where

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

damp :: [Int] -> Bool
damp xs
  | safe xs = True
  | otherwise = go [] xs
 where
  go _ [] = False
  go acc (y : ys) = safe (acc ++ ys) || go (acc ++ [y]) ys

countSafe :: ([Int] -> Bool) -> [[Int]] -> Int
countSafe f = length . filter f
