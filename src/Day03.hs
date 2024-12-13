module Day03 where

import Text.Parsec

testInput :: String
testInput = "w()xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+\nmul(32,64]then(mul(11,8)mul(8,5))"

mulParser :: Parsec String () [(Int, Int)]
mulParser = many1 (try numParser <|> junkParser)
 where
  numParser = do
    _ <- string "mul("
    a <- num
    _ <- char ','
    b <- num
    _ <- char ')'
    return (read a, read b)
  num = many1 digit

junkParser :: Parsec String () (Int, Int)
junkParser = anyChar >> return (0, 0)

parseMem :: String -> [(Int, Int)]
parseMem s =
  case parse mulParser "" s of
    Left _ -> []
    Right xs -> xs

mul :: [(Int, Int)] -> Int
mul = sum . map (uncurry (*))
