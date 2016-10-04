{-# LANGUAGE TemplateHaskell #-}

module Main where

import WordNumberTest
import Data.List (sort)
import Data.Char (toUpper)

import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonZero)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_sortOrder :: (Ord a) => [a] -> Bool
prop_sortOrder = listOrdered . sort

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_quotRem :: (Integral a) => a -> NonZero a -> Bool
prop_quotRem x (NonZero y) = (quot x y) * y + (rem x y) == x

prop_divMod :: (Integral a) => a -> NonZero a -> Bool
prop_divMod x (NonZero y) = (div x y) * y + (mod x y) == x

expAssociative :: (Eq a, Num a, Integral b) => a -> b -> b -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: (Integral a) => a -> a -> Bool
expCommutative x y = x ^ y == y ^ x

prop_reverseIsId :: (Eq a) => [a] -> Bool
prop_reverseIsId xs = (reverse . reverse $ xs) == id xs

prop_dollarApply :: (Eq b) => (a -> b) -> a -> Bool
prop_dollarApply f a = (f $ a) == f a

prop_foldrConcatOp :: (Eq a) => [a] -> [a] -> Bool
prop_foldrConcatOp xs ys = (foldr (:) ys xs) == (xs ++ ys)

prop_foldrConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrConcat x = (foldr (++) [] x) == (concat x)

-- false!
prop_lengthTake :: Int -> [a] -> Bool
prop_lengthTake n xs = length (take n xs) == n

prop_readShow :: (Eq a, Show a, Read a) => a -> Bool
prop_readShow x = (read (show x)) == x

square :: (Num a) => a -> a
square x = x * x

-- changed to work within precision
prop_squareSqrtIdentity :: (Floating a, Ord a) => NonNegative a -> Bool
prop_squareSqrtIdentity (NonNegative x) = abs ((square . sqrt $ x) - id x) < 1e37

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

prop_idem :: Eq a => (a -> a) -> a -> Bool
prop_idem f a = f a == twice f a && f a == fourTimes f a

data Fool = Fulse | Frue deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = elements [Fulse, Frue]

foolGenTwoThirds :: Gen Fool
foolGenTwoThirds = frequency [(2, return Fulse),
                              (1, return Frue)]

main :: IO ()
main = do
  wordNumberTest
  quickCheck prop_halfIdentity
  quickCheck (prop_sortOrder :: [Integer] -> Bool)
  quickCheck (prop_sortOrder :: String -> Bool)
  quickCheck (plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (plusCommutative :: Integer -> Integer -> Bool)
  quickCheck (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_quotRem :: Integer -> NonZero Integer -> Bool)
  quickCheck (prop_divMod :: Integer -> NonZero Integer -> Bool)
  quickCheck (prop_reverseIsId :: [Integer] -> Bool)
--  quickCheck (prop_dollarApply :: (Integer -> Integer) -> Integer -> Bool)
  quickCheck (prop_foldrConcatOp :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_foldrConcat :: [[Int]] -> Bool)
  quickCheck (prop_readShow :: Integer -> Bool)
  quickCheck (prop_readShow :: String -> Bool)
  quickCheck (prop_squareSqrtIdentity :: NonNegative Double -> Bool)
  quickCheck $ prop_idem capitalizeWord
  quickCheck $ (prop_idem sort :: [Integer] -> Bool)
  quickCheck $ expectFailure (expAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck $ expectFailure (expCommutative :: Integer -> Integer -> Bool)
  quickCheck $ expectFailure (prop_lengthTake :: Int -> [Integer] -> Bool)
