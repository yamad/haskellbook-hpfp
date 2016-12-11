{-# OPTIONS_GHC
  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

-- | Chapter 22, Reader, Chapter Exercises
module Ch22_Exercises where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 = liftA2 (,) ys zs

x3 = liftA2 (,) z' z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)


main :: IO ()
main = do
  -- 1.
  print $ getAll . foldMap All $ sequA 10
  print $ foldr (&&) True (sequA 10)
  -- 2.
  print $ sequA (fromMaybe 0 s')
  -- 3.
  print $ bolt (fromMaybe 0 ys)
