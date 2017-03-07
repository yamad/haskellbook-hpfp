{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Ch28_Queue

import Test.QuickCheck
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Random
import Data.Random.Extras (choices)
import Control.DeepSeq
import Criterion.Main

instance Arbitrary a =>
         Arbitrary (EQueue a) where
  arbitrary = EQueue <$> arbitrary <*> arbitrary

instance Arbitrary a =>
         Arbitrary (ListQueue a) where
  arbitrary = ListQueue <$> arbitrary

instance Arbitrary a =>
         Arbitrary (SeqQueue a) where
  arbitrary = SeqQueue <$> arbitrary


-- | Validates that the first element pushed is the first element popped
prop_FIFO :: (Eq a, Queue q) => q a -> [a] -> Bool
prop_FIFO _ [] = True
prop_FIFO q xs@(x:_) = (fst <$> pop (foldl' (flip push) q xs)) == (Just x)

----------------------------------------------------------
-- * Run a random series of pushes and pops from the queue
----------------------------------------------------------

-- | Stream of `n` pushes (p = 3/4) and pops (p = 1/4)
pushPopStream :: Queue q => Int -> IO [q Int -> q Int]
pushPopStream n =
  runRVar (choices n [push 0, push 1, push 2, popRetQueue]) StdRandom
  where
    -- | queue resulting from a pop, discarding popped element
    popRetQueue
      :: Queue q
      => q a -> q a
    popRetQueue q
      | isEmpty q = q
      | otherwise = fromJust $ snd <$> pop q


-- | Run push/pop sequence
runPushPop :: Queue q => Int -> IO (q Int)
runPushPop n = foldr id empty <$> pushPopStream n

runPushPopLQ :: Int -> IO (ListQueue Int)
runPushPopLQ = runPushPop

runPushPopQ :: Int -> IO (EQueue Int)
runPushPopQ = runPushPop

runPushPopSQ :: Int -> IO (SeqQueue Int)
runPushPopSQ = runPushPop


-- * instances for criterion benchmark

instance NFData (EQueue a) where
  rnf = flip seq $ ()

instance NFData (ListQueue a) where
  rnf = flip seq $ ()

instance NFData (SeqQueue a) where
  rnf = flip seq $ ()


main :: IO ()
main = do
  quickCheck $ prop_FIFO (empty :: ListQueue Int)
  quickCheck $ prop_FIFO (empty :: EQueue Int)
  defaultMain
    [ bench "list-based queue, push/pop" $ nfIO (runPushPopLQ 1000)
    , bench "efficient queue, push/pop" $ nfIO (runPushPopQ 1000)
    , bench "sequence queue, push/pop" $ nfIO (runPushPopSQ 1000)
    ]
