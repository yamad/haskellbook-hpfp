{-# LANGUAGE TemplateHaskell #-}

module HPFPChap07 where

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f a = a + 1

addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \a -> a + 1


addFive x y = (if x > y then y else x) + 5
addFive' = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x
mflip' f x y = f y x


-- Variety Pack
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- k :: (a, b) -> a
-- k2 :: [Char], different from both k1 and k3
-- k1 and k3 will return 3

-- Case Practice
functionC x y = if (x > y) then x else y
functionC' x y = case x > y of
                   True  -> x
                   False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n = case even n of
                  True  -> n+2
                  False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- dodgy 1 1 --> 11
-- dodgy 2 2 --> 22
-- dodgy 1 2 --> 21
-- dodgy 2 1 --> 12
-- oneIsOne 1 --> 11
-- oneIsOne 2 --> 21
-- oneIsTwo 1 --> 21
-- oneisTwo 2 --> 22
-- oneIsOne 3 --> 31
-- oneIsTwo 3 --> 23

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100


pal xs
  | xs == reverse xs = True
  | otherwise        = False


numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1


tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10


digitGetter :: Integral a => a -> a -> a
digitGetter a = flip mod 10 . fst . flip divMod a

tensDigit' :: Integral a => a -> a
tensDigit' = flip mod 10 . fst . flip divMod 10

hunsD :: Integral a => a -> a
hunsD = digitGetter 100


foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
                   True  -> x
                   False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b     = x
  | not b = y


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
