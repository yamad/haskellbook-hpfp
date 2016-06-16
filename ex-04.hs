module HPFPChap4 where

-- exercise 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- exercise 9. use if-then-else to write absolute value function
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else (- x)

-- exercise 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))


-- Reading syntax

x = (+)
-- F xs = w 'x' 1
--      where w = length xs
g xs = w `x` 1
  where w = length xs

-- \ X = x
-- (\x -> x)      -- corrected

-- \ x : xs -> x
-- (\(x:xs) -> x) -- corrected

-- f (a b) = A
h (a, b) = a

main :: IO ()
main = do
  print $ g "ABCD"
  print $ (\x -> x) 5
  print $ (\(x:xs) -> x) [1,2,3]
  print $ h (1, 2)
