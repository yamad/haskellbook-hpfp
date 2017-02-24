{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Chapter 27, Non-strictness, Chapter Exercises
module Ch27_Exercises where

--import Ch27_StrictList          -- 1.

-- * What will :sprint output?

-- | 1.
--
-- >>> let x = 1
-- >>> :sprint x
-- x = _

-- | 2.
--
-- >>> let x = ['1']
-- >>> :sprint x
-- x = "1"

-- | 3.
--
-- >>> let x = [1]
-- >>> :sprint x
-- x = _

-- | 4.
--
-- >>> let x = 1 :: Int
-- >>> :sprint x
-- x = 1

-- | 5.
--
-- >>> let f = \x -> x
-- >>> let x = f 1
-- >>> :sprint x
-- x = _
--
-- >>> x
-- 1
--
-- >>> :sprint x
-- x = 1


----------------------------------------------------
-- * Will printing this expression result in bottom?
----------------------------------------------------

-- | 1.
--
-- >>> snd (undefined, 1)
-- 1

-- | 2.
--
-- >>> let x = undefined
-- >>> let y = x `seq` 1 in snd (x, y)
-- undefined

-- | 3.
--
-- >>> length $ [1..5] ++ undefined
-- undefined

-- | 4.
--
-- >>> length $ [1..5] ++ [undefined]
-- 6

-- | 5.
--
-- >>> const 1 undefined
-- 1

-- | 6.
--
-- >>> const 1 (undefined `seq` 1)
-- 1

-- | 7.
--
-- >>> const undefined 1
-- undefined


-------------------------------
-- * Make the expression bottom
-------------------------------

-- | 1.
x = undefined
y = "blah"

mainBang = do
  print $ let forceTuple !a !b = (a, b) in snd $ forceTuple x y

mainSeq = do
  print (x `seq` (snd (x, y)))

main = mainSeq
