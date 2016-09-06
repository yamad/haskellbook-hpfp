module DatabaseItem where

import Data.Time
import Data.List (sort)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate x) -> x) . filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber x) -> x) . filter isDbNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . sort . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sum nums) / fromIntegral (length nums)
  where nums = filterDbNumber xs

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
fibs20 = take 20 fibs
fibsBelow100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [1..]

main :: IO ()
main = do
  print $ (length . filterDbDate $ theDatabase) == 2
  print $ (length . filterDbNumber $ theDatabase) == 1
