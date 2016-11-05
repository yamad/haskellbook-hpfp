-- | Chatper 20, Foldable
module Ch20 where

import Data.Monoid
import qualified Data.Foldable as F

--- Exercises: Library Functions

-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . F.foldMap Sum

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . F.foldMap Product

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . F.foldMap (Any . (== a))

-- 4.
minimum
  :: (Foldable t, Ord a)
  => t a -> Maybe a
minimum = foldr minMaybe Nothing
  where
    minMaybe a Nothing = Just a
    minMaybe a (Just b) = Just $ min a b

-- 5.
maximum
  :: (Foldable t, Ord a)
  => t a -> Maybe a
maximum = foldr maxMaybe Nothing
  where
    maxMaybe a Nothing = Just a
    maxMaybe a (Just b) = Just $ max a b

-- 6.
-- if there is any structure, the function runs and returns False
null
  :: (Foldable t)
  => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7.
length
  :: (Foldable t)
  => t a -> Int
length = foldr (\_ n -> n + 1) 0

-- 8.
toList
  :: (Foldable t)
  => t a -> [a]
toList = foldr (\a b -> a : b) []

-- 9.
fold
  :: (Foldable t, Monoid m)
  => t m -> m
fold = F.foldMap id

-- 10.
foldMap
  :: (Foldable t, Monoid m)
  => (a -> m) -> t a -> m
foldMap f = foldr (\a b -> f a `mappend` b) mempty
