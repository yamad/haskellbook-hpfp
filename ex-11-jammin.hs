module Jammin where

import Data.List (sortBy, groupBy)

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

data JamJars = Jam { fruit :: Fruit
                   , count :: Int }
                   deriving (Eq, Show, Ord)

-- JamJars cardinality is: fruit x Int, 4 x Int

row1 = Jam Peach 100
row2 = Jam Plum 200
row3 = Jam Blackberry 50
row4 = Jam Apple 50
row5 = Jam Peach 50
row6 = Jam Blackberry 100
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = sum . map count

compareCount :: JamJars -> JamJars -> Ordering
compareCount (Jam _ n) (Jam _ n') = compare n n'

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

mostRow :: [JamJars] -> JamJars
mostRow = foldr1 (\a b -> if compareCount a b == GT then a else b)

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\a b -> compareKind a b == EQ) . sortJam

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
