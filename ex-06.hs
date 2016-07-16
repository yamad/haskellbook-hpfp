module HPFPChap06 where

import Data.List

---------------------
-- Does it typecheck?
---------------------

-- 1. needs `deriving Show`
data Person = Person Bool
            deriving Show

printPerson :: Person -> IO ()
printPerson = print


-- 2. needs `deriving Eq`
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
               then Blah
               else x

-- 3. a) settleDown accepts Mood data constructors: Blah or Woot
--    b) type error. 9 is a (Num a) and can't be a Mood
--    c) type error. Mood does not implement Ord (ordering)

-- 4. yes, although `s1` is not a sentence, but a function of type
-- Object -> Sentence
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-------------------
-- Given a datatype
-------------------
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- 1. won't typecheck because
--      * "chases" is a String, but should be a Rocks
--      * True is a Bool, but should be a Yeah
-- phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- 2. yes, typechecks
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. yes, typechecks. Papu is an instance of Eq
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. no, won't typecheck. Papu is not an instance of Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'


------------------
-- Match the types
------------------

-- 1. cannot replace with `i :: a`. Num a is expected because 1 is a
-- numeric literal
i :: Num a => a
-- can't be
-- i :: a
i = 1


-- 2
f :: Float
-- can't be
-- f :: Num a => a. 1.0 cannot be any Num
f = 1.0


-- 3
-- g :: Float
-- can be
g :: Fractional a => a
g = 1.0


-- 4
-- h :: Float
-- can be
h :: RealFrac a => a
h = 1.0

-- 5
-- freud :: a -> a
-- can be
freud :: Ord a => a -> a
freud x = x

-- 6
-- freud' :: a -> a
-- can be
freud' :: Int -> Int
freud' x = x

-- 7
myX = 1 :: Int
sigmund :: Int -> Int
-- can't be
-- sigmund :: a -> a
sigmund x = myX

-- 8
sigmund' :: Int -> Int
-- can't be
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- 9
--jung :: Ord a => [a] -> a
-- can be
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10
-- young :: [Char] -> Char
-- can be
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- can't be
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)


-------------------
-- Type-Kwon-Do Two
-------------------

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f k a = (+) (f a) (fromInteger k)
