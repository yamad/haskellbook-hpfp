-- | Chapter 23, State
module Ch23 where

import System.Random
import Control.Monad

--- Exercises: Roll Your Own
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN goal g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= goal = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged goal g = go 0 0 [] g
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count rolls gen
          | sum >= goal = (count, rolls)
          | otherwise =
            let (roll, nextGen) = randomR (1, 6) gen
            in go (sum + roll) (count + 1) (intToDie roll : rolls) nextGen


--- 23.6 Write State for yourself

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi $
    \s ->
       let (a, s') = g s
       in (f a, s')

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) =
    Moi $
    \s ->
       let (f', s') = f s
           (a, s'') = g s'
       in (f' a, s'')

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g =
    Moi $
    \s ->
       let (a, s') = f s
           (Moi sb) = g a
       in sb s'
