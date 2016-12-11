module Ch23_FizzBuzz where

import Control.Monad.Trans.State
import qualified Data.DList as DL

--- 23.7 Get a coding job with one weird trick

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Fizz"
  | n `mod` 3 == 0 = "Buzz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo beg end
  | end < beg  = fizzbuzzFromTo end beg
  | beg == end = [fizzBuzz beg]
  | otherwise  = fizzBuzz beg : fizzbuzzFromTo (beg + 1) end

main :: IO ()
main = do
  mapM_ putStrLn $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
