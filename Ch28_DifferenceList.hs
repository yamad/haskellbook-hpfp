-- | Difference List
--
-- Encodes a list as the sequence of steps that produces the list from
-- smaller lists
module Ch28_DifferenceList where

import Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

empty :: DList a
empty = DL $ \_ -> []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL $ \_ -> [a]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = flip unDL []
{-# INLINE toList #-}

-- Prepend a single element to a dlist
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((++ [x]) . unDL xs)
{-# INLINE snoc #-}

-- Append dlists
append :: DList a -> DList a -> DList a
append (DL fa) (DL fb) = DL $ fa . fb
{-# INLINE append #-}


-- * Benchmark
schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

dlistBenchmark :: IO ()
dlistBenchmark =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDlist 123456
    ]
