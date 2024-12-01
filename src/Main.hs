module Main where

-- import Day01

runDay :: (String, String -> Int) -> IO ()
runDay (path, f) = do
  udata <- readFile path
  let result = f udata
  print result

-- d01p1 :: (String, String -> Int)
-- d01p1 = ("data/d01p1.txt", f . g)

main :: IO ()
main = putStrLn "Hello, Haskell!"
