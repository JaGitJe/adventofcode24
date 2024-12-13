module Day10 where

import Data.List (nub)
import Data.Maybe (isJust)
import Day05 (wspace)
import Text.Parsec

testInput :: String
testInput = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"

getMap :: String -> [[Int]]
getMap s = case parse lineParser "" s of
  Right m -> m
  Left _ -> error "Input is broken."

lineParser :: Parsec String () [[Int]]
lineParser = many1 sglDigit `endBy1` wspace
 where
  sglDigit = digit >>= \d -> return (read [d])

valAt :: [[Int]] -> (Int, Int) -> Maybe Int
valAt m (x, y)
  | validPos m (x, y) = Just (m !! y !! x)
  | otherwise = Nothing

validPos :: [[Int]] -> (Int, Int) -> Bool
validPos m (x, y) = y >= 0 && (length m > y) && x >= 0 && (length (m !! y) > x)

countTrails :: [[Int]] -> Int
countTrails m =
  sum
    [ score (x, y)
    | x <- [0 .. length m - 1]
    , y <- [0 .. length (head m) - 1]
    , valAt m (x, y) == Just 0
    ]
 where
  score (x, y) = length $ filter ((Just 9 ==) . valAt m) (getTrails m (x, y))

getTrails :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getTrails m (x, y) = nub $ dfs [(x, y)] (x, y)
 where
  dfs visited cur = dfs' (cur : visited) (directions cur)
  dfs' visited [] = visited
  dfs' visited (t : ts)
    | t `notElem` visited = dfs visited t ++ dfs' visited ts
    | otherwise = dfs' visited ts
  directions cur@(a, b) =
    let u = (a, b - 1)
        r = (a + 1, b)
        d = (a, b + 1)
        l = (a - 1, b)
     in [next | next <- [u, r, d, l], isValidTrail cur next]
  isValidTrail cur next = isJust (valAt m cur) && isSlope cur next
  isSlope cur next = case (valAt m cur, valAt m next) of
    (Just p, Just n) -> p < n && abs (p - n) == 1
    _ -> False
