module Day06 where

import Data.List
import Data.List.Index
import Day05 (wspace)
import Text.Parsec

testInput :: String
testInput = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

getFloorplan :: String -> [String]
getFloorplan s = case parse (lineParser `endBy1` wspace) "" s of
  Right m -> m
  Left _ -> error "input is broken."
 where
  lineParser = many1 (noneOf "\n") >>= \s' -> return s'

data Direction = North | East | South | West
  deriving (Show)
type Plan = [String]

getFacing :: Char -> Maybe Direction
getFacing c = case c of
  '^' -> Just North
  '>' -> Just East
  'v' -> Just South
  '<' -> Just West
  _ -> Nothing

startLocation :: Int -> [String] -> Maybe (Int, Int)
startLocation _ [] = Nothing
startLocation y (s : m) = case '^' `elemIndex` s of
  Nothing -> startLocation (y + 1) m
  Just x -> Just (x, y)

isValid :: Plan -> (Int, Int) -> Bool
isValid m (x, y) =
  0 <= y
    && y < length m
    && 0 <= x
    && x < length (m !! y)

charAt :: Plan -> (Int, Int) -> Maybe Char
charAt m (x, y)
  | isValid m (x, y) = Just (m !! y !! x)
  | otherwise = Nothing

patrolPositions :: Plan -> Int
patrolPositions plan = go (plan, start, startDir)
 where
  start = case startLocation 0 plan of
    Just (x, y) -> (x, y)
    _ -> error "start location broken."
  startDir = case charAt plan start of
    Just c -> case getFacing c of
      Just d -> d
      _ -> error "start facing broken."
    _ -> error "start direction broken."
  go (p, s, d)
    | isValid p s = go (moveGuard (p, s, d))
    | otherwise = length $ concatMap (filter ('X' ==)) p

moveGuard :: (Plan, (Int, Int), Direction) -> (Plan, (Int, Int), Direction)
moveGuard (prevPlan, prevPos, prevDir) =
  if isObstruction nextPos
    then
      moveGuard (nextPlan, prevPos, nextDir)
    else (nextPlan, nextPos, prevDir)
 where
  nextPlan =
    let (x, y) = prevPos
     in modifyAt y (modifyAt x (const 'X')) prevPlan
  nextPos =
    let (x, y) = prevPos
     in case prevDir of
          North -> (x, y - 1)
          East -> (x + 1, y)
          South -> (x, y + 1)
          West -> (x - 1, y)
  isObstruction (x', y') = case charAt prevPlan (x', y') of
    Just '#' -> True
    Just _ -> False
    Nothing -> False
  nextDir = case prevDir of
    North -> East
    East -> South
    South -> West
    West -> North
