{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Chapter 18, Monad, In-Chapter Exercise Tests
module Ch18_Tests where

import Ch18

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

type SumIntTrigger = Sum Int (Int, Int, Int)

main :: IO ()
main = do
  let trigger = undefined :: SumIntTrigger
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
