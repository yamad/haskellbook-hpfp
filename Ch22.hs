{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Chapter 22, Reader
module Ch22 where

import Control.Applicative
import Control.Monad
import Data.Char

--- 22.2 A new beginning
boop = (*2)
doop = (+10)

-- Functor context: bip and bloop are equivalent
bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

-- Applicative context: bbop and duwop are equivalent
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- Monad context: works like bbop/duwop. no change from Applicative
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)



--- Short Exercise: Warming Up
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

-- monadic version with do
tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return (c, r)

-- monadic version with bind
tupledMM :: [Char] -> ([Char], [Char])
tupledMM = cap >>= (\c -> rev >>= (\r -> return (,) c r))



--- Exercise: Ask
newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

ask :: Reader a a
ask = Reader id



--- Demonstrating the function applicative
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "BigBird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address


--- Exercise: Reading Comprehension

-- 1. Write liftA2
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = fmap f a <*> b

-- 2.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b)
       -> Reader r a
       -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader (\r -> rab r (ra r))


--- Exercise: Reader Monad

-- 1.
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> (runReader . aRb) (ra r) r

-- 2.
getDogRM :: Person -> Dog
getDogRM = liftM2 Dog dogName address

getDogRM' :: Person -> Dog
getDogRM' =
  dogName >>= (\name -> (address >>= (\addy -> return $ Dog name addy)))
