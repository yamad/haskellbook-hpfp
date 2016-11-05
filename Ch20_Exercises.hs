-- | Chatper 20, Foldable, Exercises
module Ch20_Exercises where

import Data.Monoid ((<>))

-- 1.
data Constant a b =
  Constant a
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap _ _ = mempty


-- 2.
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


-- 3.
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


-- 4.
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'


-- 5.
data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b c d) = f b <> f c <> f d



-- Write a filter function for Foldable using foldMap
filterF
  :: (Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF p =
  foldMap
    (\a ->
        if p a
          then pure a
          else mempty)
