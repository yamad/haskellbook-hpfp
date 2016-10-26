{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Chapter 18, Monad, Exercises
module Ch18_Exercises where

import Control.Monad (ap, join, liftM, liftM2)

--- Write Monad instances for the following types

-- 1.
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap = liftM

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) = ap

instance Monad Nope where
  _ >>= _ = NopeDotJpg


-- 2.
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap = liftM

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) = ap

instance Monad (PhhhbbtttEither b) where
  (Right' b) >>= _ = Right' b
  (Left' a)  >>= f = f a


-- 3.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = Identity
  (<*>) = ap

instance Monad Identity where
  (Identity a) >>= f = f a


-- 4.
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

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

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons (f a) (fmap f r)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  funcs <*> vals = flatMap (flip flatMap vals) liftedFuncs
    where liftedFuncs = fmap (pure .) funcs

instance Monad List where
  Nil >>= _ = Nil
  l >>= f = (concat' . fmap f) l


--- Write the following functions using the methods provided by Monad and Functor

-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = flip mapM

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id
