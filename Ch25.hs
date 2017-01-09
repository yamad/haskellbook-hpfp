{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- | Chapter 25, Composing types (Monad Transformers I)
module Ch25 where

import Control.Applicative (liftA2)

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)


-- Functor
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) =>
         Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


-- Applicative
instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- lift Applicative apply over the `f` Applicative layer
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a


-- Exercises: Compose Instances

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga



--- And now for something completely different (Bifunctor)

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1.
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)


-- 2.
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)


-- 3.
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)


-- 4.
data SuperDrei a b c = SuperDrei a b deriving Show

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)


-- 5.
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a


-- 6.
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)


-- 7.
data Either' a b
  = Left' a
  | Right' b
  deriving Show

instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' $ f a
  bimap _ f (Right' b) = Right' $ f b



--- IdentityT
newtype Identity' a = Identity'
  { runIdentity' :: a
  } deriving (Eq, Show)

newtype IdentityT f a = IdentityT
  { runIdentityT :: f a
  } deriving (Eq, Show)


instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)

instance (Functor m) =>
         Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)


instance Applicative Identity' where
  pure = Identity'
  (Identity' f) <*> (Identity' a) = Identity' (f a)

instance (Applicative m) =>
         Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)


instance Monad Identity' where
  return = pure
  (Identity' a) >>= f = f a

instance (Monad m) =>
         Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
