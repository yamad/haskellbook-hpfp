module Arith3Broken where

main :: IO ()
main = do
  print $ (1 + 2 :: Integer)
  print $ (10 :: Integer)
  print $ (negate (-1) :: Integer)
  print $ let blah = negate 1 in (((+) 0 blah) :: Integer)
