module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "unknown"

digits :: Int -> [Int]
digits n = go n []
  where go d acc
          | d == 0 = acc
          | otherwise = go (d `div` 10) ((d `mod` 10) : acc)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n

main :: IO ()
main = do
  print $ wordNumber 12345
