{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Chapter 18, Monad, Chapter Exercise Tests
module Ch18_ExercisesTests where

import Ch18_Exercises

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Right' b), (3, return $ Left' a)]

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Nil), (3, return $ Cons a (Cons b Nil))]


instance EqProp (Nope a) where
  _ =-= _ = property True
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq
instance (Eq a) => EqProp (Identity a) where (=-=) = eq
instance (Eq a) => EqProp (List a) where (=-=) = eq


jTest :: Bool
jTest =
  ((j [[1, 2], [], [3]]) == [1,2,3])
  && ((j (Just (Just 1))) == Just 1)
  && ((j (Just Nothing)) == (Nothing :: Maybe Int))
  && ((j Nothing) == (Nothing :: Maybe Int))


main :: IO ()
main = do
  putStrLn "\n-- Nope ----"
  let nopeIntTrigger = NopeDotJpg :: Nope (Int, Int, Int)
  quickBatch $ functor nopeIntTrigger
  quickBatch $ applicative nopeIntTrigger
  quickBatch $ monad nopeIntTrigger

  putStrLn "\n\n-- PhhhbbtttEither ----"
  let peitherIntTrigger = undefined :: PhhhbbtttEither Int (Int, Int, Int)
  quickBatch $ functor peitherIntTrigger
  quickBatch $ applicative peitherIntTrigger
  quickBatch $ monad peitherIntTrigger

  putStrLn "\n\n-- Identity ----"
  let identityIntTrigger = undefined :: Identity (Int, Int, Int)
  quickBatch $ functor identityIntTrigger
  quickBatch $ applicative identityIntTrigger
  quickBatch $ monad identityIntTrigger

  putStrLn "\n\n-- List ----"
  let listIntTrigger = undefined :: List (Int, Int, Int)
  quickBatch $ functor listIntTrigger
  quickBatch $ applicative listIntTrigger
  quickBatch $ monad listIntTrigger

  print $ jTest
