{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HPFPChap11 where

import Data.Char (toUpper, isUpper, toLower)
import Data.List (intercalate, elemIndex, elemIndices, sort, sortOn, group)

data Price =
    Price Integer
    deriving (Eq,Show)

data Size =
    Size Double
    deriving (Eq,Show)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq,Show)

data Airline
    = PapuAir
    | CatapltsR'Us
    | TakeYourChancesUnited
    deriving (Eq,Show)

data Vehicle
    = Car Manufacturer
          Price
    | Plane Airline
            Size
    deriving (Eq,Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 2000.15)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "Don't know plane manufacturers"

class TooMany a  where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n,xs) = length xs > n

instance TooMany (Int, Int) where
    tooMany (m,n) = tooMany (Goats (m + n))

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (m,n) = tooMany (m + n)

newtype Goats =
    Goats Int
    deriving (Eq,Show,TooMany)

data OperatingSystem
    = GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq,Show)

data ProgrammingLanguage
    = Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq,Show)

data Programmer = Programmer
    { os :: OperatingSystem
    , lang :: ProgrammingLanguage
    } deriving (Eq,Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
    [Programmer os lang | os <- allOperatingSystems
                        , lang <- allLanguages]

data Quantum
    = Yes
    | No
    | Both
    deriving (Eq,Show)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = True
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = True
convert7 No = False
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

data Quad
    = One
    | Two
    | Three
    | Four
    deriving (Eq,Show)

-- 8 forms, Either is sum type, and Quad has 4 forms, so 4 + 4 = 8
eQuad :: Either Quad Quad
eQuad = undefined

-- 16 forms, Product type, 4 x 4
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- 4^4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined

-- 3 x 3 x 3 = 27
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- (2^2)^2 = 2^4 = 16
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- (4^4)^2 = 2^16 = 65536
fTwo :: Bool -> Quad -> Quad
fTwo = undefined

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : (preorder l ++ preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l ++ [a] ++ inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder is fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node l a r) = f a (foldTree f (foldTree f acc r) l)

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' xs@(x:xs') (y:ys)
  | x == y = isSubsequenceOf' xs' ys
  | otherwise = isSubsequenceOf' xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capTuple . words
  where capTuple xs@(x:xs') = (xs, toUpper x : xs')

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeSentence :: String -> String
capitalizeSentence = capitalizeWord

spanSentences :: String -> ([String], String)
spanSentences "" = ([], "")
spanSentences (' ':xs) = spanSentences xs -- skip leading space
spanSentences xs = case span (/= '.') xs of
                     (f, "") -> ([f], "")
                     (f, _:rs)  -> let (f', r') = spanSentences rs in ((f++"."):f', r')

splitSentences :: String -> [String]
splitSentences = fst . spanSentences

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . map capitalizeSentence . splitSentences

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
