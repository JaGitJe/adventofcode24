module Day07 (getEquations, sumSolvable) where

import Day05 (wspace)
import Text.Parsec

testInput :: String
testInput = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"

getEquations :: String -> [(Int, [Int])]
getEquations s = case parse (lineParser `endBy1` wspace) "" s of
  Right ns -> ns
  Left _ -> error "input is broken."
 where
  lineParser =
    resultParser
      >>= \res ->
        numParser `sepBy1` char ' '
          >>= \ns -> return (res, ns)
  resultParser =
    many1 digit
      >>= \a ->
        string ": "
          >> return (read a)
  numParser =
    many1 digit
      >>= return . read

sumSolvable :: [(Int, [Int])] -> Int
sumSolvable = sum . map fst . filter isSolvable

isSolvable :: (Int, [Int]) -> Bool
isSolvable (_, []) = False
isSolvable (r, x : xs) = isSolvable' x xs
 where
  isSolvable' r' [] = r' == r
  isSolvable' r' (y : ys) = isSolvable' (r' * y) ys || isSolvable' (r' + y) ys
