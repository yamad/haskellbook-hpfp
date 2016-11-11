{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Chapter 21, Traversable, Chapter Exercise Tests
module Ch21_ExercisesTests where

import Ch21_Exercises

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

instance Arbitrary a =>
         Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Arbitrary a =>
         Arbitrary (Optional a) where
  arbitrary = frequency [(1, return $ Nada), (3, Yep <$> arbitrary)]

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = sized arbList

arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 = return Nil
arbList n =
  frequency [(1, return Nil), (4, Cons <$> arbitrary <*> (arbList (n - 1)))]

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

instance Arbitrary a =>
         Arbitrary (S Optional a) where
  arbitrary = do
    o <- arbitrary
    a <- arbitrary
    return $ S o a

instance Arbitrary a =>
         Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = Leaf <$> arbitrary
arbTree n =
  frequency
    [ (1, return $ Empty)
    , (1, Leaf <$> arbitrary)
    , (2, Node <$> (arbTree (n `div` 2)) <*> arbitrary <*> (arbTree (n `div` 2)))
    ]


instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Eq a => EqProp (Constant a b) where
  Constant a =-= Constant a' = a `eq` a'
instance Eq a => EqProp (Optional a) where (=-=) = eq
instance Eq a => EqProp (List a) where (=-=) = eq
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq
instance Eq a => EqProp (S Optional a) where (=-=) = eq
instance Eq a => EqProp (Tree a) where (=-=) = eq


main :: IO ()
main = do
  putStrLn "Identity"
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

  putStrLn "\nConstant a"
  quickBatch $
    traversable
      (undefined :: Constant (Int, Int, [Int]) (String, String, String))

  putStrLn "\nOptional"
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))

  putStrLn "\nList"
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

  putStrLn "\nThree"
  quickBatch $
    traversable
      (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]))

  putStrLn "\nThree'"
  quickBatch $
    traversable
      (undefined :: Three' (Int, [Int]) (Int, Int, [Int]))

  putStrLn "\nS n a"
  quickBatch $ traversable (undefined :: S Optional (Int, Int, [Int]))

  putStrLn "\nTree"
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
