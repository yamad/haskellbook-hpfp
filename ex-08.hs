module HPFPChap08 where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"


-- dividedBy 15 2
-- go 15 2 0
-- go 13 2 1
-- go 11 2 2
-- go  9 2 3
-- go  7 2 4
-- go  5 2 5
-- go  3 2 6
-- go  1 2 7
-- (7, 1)

sumIntTo :: (Eq a, Num a) => a -> a
sumIntTo end = go end 0
  where go n acc
          | n == 0    = acc
          | otherwise = go (n - 1) (acc + n)

multiplyIterate :: (Integral a) => a -> a -> a
multiplyIterate m1 m2 = go m1 m2 0
  where go a b acc
          | a == 0    = 0
          | b == 0    = acc
          | b < 0     = go a (b + 1) (acc - a)
          | otherwise = go a (b - 1) (acc + a)

data DividedResult = Result Integer
                   | DividedByZero deriving (Show)

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom = go num denom 0
  where go n d count
          | d == 0         = DividedByZero
          | abs n < abs d  = Result count
          | n < 0 && d > 0 = go (n + d) d (count - 1)
          | n > 0 && d < 0 = go (n + d) d (count - 1)
          | otherwise      = go (n - d) d (count + 1)


mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ n + 11


main :: IO ()
main = do
  print $ sumIntTo 10
  print $ multiplyIterate   5    6
  print $ multiplyIterate (-5)   6
  print $ multiplyIterate (-5) (-6)
  print $ multiplyIterate   5  (-6)
  print $ dividedBy' ( 10) ( 2)
  print $ dividedBy' ( 10) (-2)
  print $ dividedBy' (-10) (-2)
  print $ dividedBy' (-10) ( 2)
  print $ dividedBy' ( 10) ( 0)
  print $ map mc91 [95..110]
