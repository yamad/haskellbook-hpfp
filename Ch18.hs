-- | Chapter 18, Monad, In-Chapter Exercises
module Ch18 where

import Control.Monad (join, ap, (>=>))


bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

twiceWhenEvenM :: [Integer] -> [Integer]
twiceWhenEvenM =
  (>>= (\x ->
           if even x
             then [x * x, x * x]
             else []))


-- Short Exercise: Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) = ap              -- use Monad instance to define applicative
--  (First a) <*> _ = First a
--  _ <*> (First a) = First a
--  (Second f) <*> (Second b) = Second $ f b

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b


-- Monadic composition
mcomp
  :: Monad m
  => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a)) -- using join/fmap

mcomp''
  :: Monad m
  => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f       -- using bind

-- Kleisli composition
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
