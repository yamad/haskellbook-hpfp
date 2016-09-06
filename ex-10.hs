{-# LANGUAGE TemplateHaskell #-}
module HPFPChap10 where

stops  = "pbtdkg"
vowels = "aeiou"

threes :: [String]
threes = [ x:y:z:[] | x <- stops, y <- vowels, z <- stops ]

threesP :: [String]
threesP = [ 'p':y:z:[] | y <- vowels, z <- stops ]

seekritFunc x = div (sum (map length (words x)))
                    (length (words x))

seekritFunc' x = (/) (fromIntegral (sum (map length (words x))))
                     (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' = foldr (\a b -> if a == True then True else b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> if x == a then True else b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap' id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldr1 (\a b -> if cmp a b == GT then a else b)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp = foldr1 (\a b -> if cmp a b == LT then a else b)

main = do
  --foldr (++) ["woot", "WOOT", "woot"]
  print $ foldr (++) "" ["woot", "WOOT", "woot"]

  --foldr max [] "fear is the little death"
  print $ foldr max ' ' "fear is the little death"

  --foldr and True [False, True]
  print $ foldr (&&) True [False, True]

  --foldr (||) True [False, True]
  print $ foldr (||) True [False, True]

  --foldl ((++) . show) "" [1..5]
  print $ foldl (flip ((++) . show)) "" [1..5]

  --foldr const 'a' [1..5]
  print $ foldr (flip const) 'a' [1..5]
  print $ foldl const 'a' [1..5]

  --foldr const 0 "tacos"
  print $ foldr const ' ' "tacos"

  --foldl (flip const) 0 "burritos"
  print $ foldl (flip const) ' ' "burritos"

  --foldl (flip const) 'z' [1..5]
  print $ foldl (flip const) 0 [1..5]

  print $ myOr [False, False, False] == False
  print $ myOr [False, True,  False] == True
  print $ myAny even [1, 3, 5] == False
  print $ myAny odd  [1, 3, 5] == True
  print $ myElem 1 [1..10] == True
  print $ myElem 5 [1..10] == True
  print $ myElem 11 [1..10] == False
  print $ myElem 1 [2..10] == False
  print $ myElem' 1 [1..10] == True
  print $ myElem' 5 [1..10] == True
  print $ myElem' 11 [1..10] == False
  print $ myElem' 1 [2..10] == False
