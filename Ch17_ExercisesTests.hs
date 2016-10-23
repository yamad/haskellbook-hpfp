{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Chapter 17, Applicative Chapter Exercise Tests
module Ch17_ExercisesTests where

import Ch17_Exercises

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b


instance Eq a => EqProp (Pair a) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

main :: IO ()
main = do
  putStrLn "Pair a"
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))
  putStrLn "Two a b"
  quickBatch $ applicative (undefined :: Two [Int] (Int, Int, Int))
  putStrLn "Three a b c"
  quickBatch $ applicative (undefined :: Three [Int] [Int] (Int, Int, Int))
  putStrLn "Three' a b"
  quickBatch $ applicative (undefined :: Three' [Int] (Int, Int, Int))
  putStrLn "Four a b c d"
  quickBatch $ applicative (undefined :: Four [Int] [Int] [Int] (Int, Int, Int))
  putStrLn "Four' a b"
  quickBatch $ applicative (undefined :: Four' [Int] (Int, Int, Int))
