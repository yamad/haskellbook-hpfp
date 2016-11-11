-- | Chapter 17, Applicative, In-Chapter Exercises
module Ch17 where

import Control.Applicative (liftA2)
import Data.List (elemIndex)
import Data.Monoid ((<>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z


-- 3.
x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x' y'


-- 4.
xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

-- not sure if this is right here. the name `summed` suggests the most
-- natural thing would be to sum x'' and y''. but applying sum to a
-- tuple (,) calculates the "sum" over the second element, and ignores
-- the first. so this solution typechecks, but returns (Just 5), where
-- the sum of x'' and y'' is more logically (Just 30).
summed :: Maybe Integer
summed = fmap sum $ liftA2 (,) x'' y''



-- Exercise: Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a


-- Exercise: Constant Instance
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant a <*> Constant a' = Constant (a <> a')


-- Exercise: Fixer Upper

-- 1.
hello = const <$> Just "Hello" <*> Just "World"

-- 2.
quad = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]



-- Exercise: List Applicative

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  funcs <*> vals = flatMap (flip flatMap vals) liftedFuncs
    where liftedFuncs = fmap (pure .) funcs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

-- helper functions from book
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = sized arbList

arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 = return Nil
arbList n =
  frequency [(1, return Nil), (4, Cons <$> arbitrary <*> (arbList (n - 1)))]

instance Eq a => EqProp (List a) where (=-=) = eq

testListApplicative = do
  let functions = Cons (+1) (Cons (*2) Nil)
  let values = Cons 1 (Cons 2 Nil)
  print $ (functions <*> values) == (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))



-- Exercise: ZipList Applicative
take' :: Int -> List a -> List a
take' 0 _   = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h (take' (n - 1) t)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                    in take' 3000 l
          ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . pure
  (ZipList' (Cons f Nil)) <*> (ZipList' (Cons x Nil)) = ZipList' $ Cons (f x) Nil
  (ZipList' (Cons f Nil)) <*> z = ZipList' (repeat' f) <*> z
  z <*> (ZipList' (Cons x Nil)) = z <*> ZipList' (repeat' x)
  (ZipList' fs) <*> (ZipList' xs) = ZipList' (zipWith' ($) fs xs)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)


instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

testZipListApplicative = do
  let z = ZipList' (Cons (+ 9) (Cons (* 2) (Cons (+ 8) Nil)))
  let z' = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
  print ((z <*> z') == ZipList' (Cons 10 (Cons 4 (Cons 11 Nil))))
  let z'' = ZipList' (repeat' 1)
  print ((z <*> z'') == ZipList' (Cons 10 (Cons 2 (Cons 9 Nil))))


-- Exercise: Variations on Either
data Validation e a
  = Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e =>
         Applicative (Validation e) where
  pure = Success'
  Failure' e <*> Failure' e' = Failure' $ e <> e'
  Failure' e <*> _ = Failure' e
  _ <*> Failure' e = Failure' e
  Success' f <*> Success' a = Success' $ f a

instance (Arbitrary e, Arbitrary a) =>
         Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) =>
         EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  testListApplicative
  quickBatch $ applicative (undefined :: List (String, String, Int))
  testZipListApplicative
  quickBatch $ applicative (undefined :: ZipList' (Int, String, Int))
  quickBatch $ applicative (undefined :: Validation String (Int, Int, Int))
