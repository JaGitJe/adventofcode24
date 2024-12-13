module Day05 where

import Data.Map.Strict (Map, fromListWith, (!?))
import Text.Parsec

testInput :: String
testInput =
  "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"

type Rules = Map Int [Int]
type Update = [Int]

wspace :: Parsec String () Char
wspace = try space <|> (eof >> return ' ')

inputParser :: Parsec String () ([(Int, [Int])], [[Int]])
inputParser =
  rulesParser >>= \rs ->
    char '\n'
      >> linesParser
      >>= \ls -> return (rs, ls)
 where
  numParser = many1 digit >>= return . read
  linesParser = (numParser `sepBy1` char ',') `endBy1` wspace
  rule =
    many1 digit
      >>= \a ->
        char '|'
          >> many1 digit
          >>= \b -> return (read a, [read b])
  rulesParser = many (rule <* wspace)

getManual :: String -> (Rules, [Update])
getManual s =
  let rules = parse inputParser "" s
   in case rules of
        Right (rs, ls) -> (fromListWith (++) rs, ls)
        Left _ -> error ""

filterUpdates :: Rules -> [Update] -> [Update]
filterUpdates _ [] = []
filterUpdates rs us = filter checkUpdateOrd us
 where
  checkUpdateOrd [] = True
  checkUpdateOrd [_] = True
  checkUpdateOrd (x : xs) = all (isBefore x) xs && checkUpdateOrd xs
  isBefore a b = case rs !? a of
    Just laterPages -> b `elem` laterPages
    Nothing -> False

sumMidPages :: String -> Int
sumMidPages s =
  let (rs, us) = getManual s
   in sum $ map (\up -> up !! (length up `div` 2)) $ filterUpdates rs us
