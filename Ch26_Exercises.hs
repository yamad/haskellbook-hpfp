-- | Chapter 26, Monad Transformers, Chapter Exercises
module Ch26_Exercises where

import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Ch26_FixTheCode
import Ch26_HitCounter

-- 1. and 2.
rDec
  :: Num a
  => Reader a a
rDec = reader $ subtract 1

-- 3. and 4.
rShow
  :: Show a
  => ReaderT a Identity String
rShow = reader show

-- 5.
rPrintAndInc
  :: (Num a, Show a)
  => ReaderT a IO a
rPrintAndInc =
  ReaderT $
  \r -> do
    putStrLn $ "Hi: " ++ show r
    return (r + 1)

-- 6.
sPrintIncAccum
  :: (Num a, Show a)
  => StateT a IO String
sPrintIncAccum =
  StateT $
  \s -> do
    putStrLn $ "Hi: " ++ show s
    return (show s, s + 1)
