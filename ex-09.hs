{-# LANGUAGE OverloadedStrings #-}

module HPFPChap09 where

import Data.Char (isUpper, toUpper)

-- | general enumFromTo implementation (go to integers and then back)
--
-- Enum instances must implement `toEnum` and `fromEnum` so rely on
-- these. No other functions are required, namely `succ` does not have
-- to work for all Enum inputs.
enumFromTo' :: Enum a => a -> a -> [a]
enumFromTo' x y = map toEnum $ enumerate (fromEnum x) (fromEnum y)
  where enumerate a b
          | a >  b  = []
          | a == b  = [b]
          | otherwise = a : enumerate (succ a) b

eftBool :: Bool -> Bool -> [Bool]
eftBool = enumFromTo'

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = enumFromTo'

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo'

eftChar :: Char -> Char -> [Char]
eftChar = enumFromTo'


-- explicit versions
eftBool' :: Bool -> Bool -> [Bool]
eftBool' True  False = []
eftBool' True  True  = [True]
eftBool' False False = [False]
eftBool' False True  = [False,True]

eftOrd' :: Ordering -> Ordering -> [Ordering]
eftOrd' LT LT = [LT]
eftOrd' EQ EQ = [EQ]
eftOrd' GT GT = [GT]
eftOrd' GT _  = []
eftOrd' _  LT = []
eftOrd' a  b  = a : eftOrd' (succ a) b

eftInt' :: Int -> Int -> [Int]
eftInt' a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftInt' (succ a) b

eftChar' :: Char -> Char -> [Char]
eftChar' a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftChar' (succ a) b


myWords :: String -> [String]
myWords "" = []
myWords  a = takeWhile (/= ' ') a : (myWords . drop 1 . dropWhile (/= ' ') $ a)

myLines :: String -> [String]
myLines [] = []
myLines xs = takeWhile (/='\n') xs : (myLines . drop 1 . dropWhile (/='\n') $ xs)

-- | split list at element, dropping the element
--
-- e.g. splitOn 'D' "ABCDEFG" --> ["ABC", "EFG"]
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = takeWhile (/= c) xs : (splitOn c . killTo $ xs)
  where killTo = drop 1 . dropWhile (/= c)

myWords' :: String -> [String]
myWords' = splitOn ' '

myLines' :: String -> [String]
myLines' = splitOn '\n'

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)


onlyUpper :: String -> String
onlyUpper = filter (isUpper)

capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) = toUpper x : xs

capAll :: String -> String
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

fstCapped :: String -> Char
fstCapped xs = toUpper . head $ xs

fstCapped' :: String -> Char
fstCapped' = toUpper . head

myOr :: [Bool] -> Bool
myOr = (/=[]) . filter (==True)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | a == x    = True
  | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

myReverse :: [a] -> [a]
myReverse = go []
  where go acc [] = acc
        go acc (x:xs) = go (x : acc) xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []  = error "empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:y:xs) = case f x y of
                           GT -> myMaximumBy f (x:xs)
                           _  -> myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []  = error "empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:y:xs) = case f x y of
                           LT -> myMinimumBy f (x:xs)
                           _  -> myMinimumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

main = do
  print $ mySqr == [1,4,9,16,25]
  print $ [x | x <- mySqr, rem x 2 == 0]                          == [4,16]
  print $ [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]        == []
  print $ take 5 [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50] == []
  print $ ([(x,y) | x <- mySqr, y <- myCube])
  print $ ([(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50])
  print $ (length $ [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50])
  print $ myZip [1,2,3] [4,5,6]                                   == [(1,4),(2,5),(3,6)]
  print $ myZip [1,2] [4,5,6]                                     == [(1,4),(2,5)]
  print $ myZip [1,2,3] [4]                                       == [(1,4)]
  print $ (myZip [] [1..100000000000] :: [(Int,Int)])             == []
  print $ myZip ['a'] [1..10000000000]                            == [('a',1)]
  print $ myZip [1..100] ['a'..'c']                         == [(1,'a'),(2,'b'),(3,'c')]
  print $ myZip' [1,2,3] [4,5,6]                                  == [(1,4),(2,5),(3,6)]
  print $ myZip' [1,2] [4,5,6]                                    == [(1,4),(2,5)]
  print $ myZip' [1,2,3] [4]                                      == [(1,4)]
  print $ (myZip' [] [1..100000000000] :: [(Int, Int)])           == []
  print $ myZip' ['a'] [1..10000000000]                           == [('a', 1)]
  print $ myZip' [1..100] ['a'..'c']                        == [(1,'a'),(2,'b'),(3,'c')]
  print $ onlyUpper "HbEfLrLxO"                                   == "HELLO"
  print $ capitalize' "julie"                                     == "Julie"
  print $ capAll "woot"                                           == "WOOT"
  print $ myAny even [1,3,5]                                == False
  print $ myAny odd [1,3,5]                                 == True
  print $ myElem 1 [1..10]                                  == True
  print $ myElem 1 [2..10]                                  == False
  print $ myReverse "blah"                                  == "halb"
  print $ myReverse [1..5]                                  == [5,4,3,2,1]
  print $ squish [[1..3], [4..6]]                           == [1..6]
  print $ squishMap (\x -> [1, x, 3]) [2]                   == [1,2,3]
  print $ squishMap (\x -> "WO "++[x]++" HOO ") "123"       == "WO 1 HOO WO 2 HOO WO 3 HOO "
  print $ squishAgain [[1..3], [4..6]]                      == [1..6]
