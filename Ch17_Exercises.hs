-- | Chapter 17, Applicative Chapter Exercises
module Ch17_Exercises where

import Data.Monoid ((<>))
import Control.Applicative (liftA3)

--- Specialize the types of the Applicative methods for given type

-- 1. [] (list)
-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2. IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. (,) a
-- pure :: a -> (a, a)
-- (<*>) :: (a, (a -> b)) -> (a, a) -> (a, b)

-- 4. (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)



--- Write applicative instances for the following datatypes. Validate
--- instances with QuickCheck

-- 1.
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a a') = Pair (f a) (g a')


-- 2.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- implement like tuple ((,) a)
-- first type acts as Monoid and mappends
-- second type is a function application, like Functor
instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two u f) <*> (Two v a) = Two (u <> v) (f a)


-- 3.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three u v f) <*> (Three u' v' c) = Three (u <> u') (v <> v') (f c)


-- 4.
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' u f f') <*> (Three' v b b') = Three' (u <> v) (f b) (f' b')


-- 5.
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four u v w f) <*> (Four u' v' w' d) =
    Four (u <> u') (v <> v') (w <> w') (f d)


-- 6.
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' u v w f) <*> (Four' u' v' w' b) =
    Four' (u <> u') (v <> v') (w <> w') (f b)


--- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

threes :: [String]
threes = tripleToList <$> combos stops vowels stops
  where tripleToList (a,b,c) = a:b:c:[]
