module HPFPChap5 where

-- type signature constrains behavior. we know nothing else about
-- parameter `a` so all we can do is return it.
myId :: a -> a
myId a = a

-- only two possible implementations of `a -> a -> a`, return first or second
myId3 :: a -> a -> a
myId3 a1 _  = a1

myId4 :: a -> a -> a
myId4 _  a2 = a2

-- implement a -> b -> b. only one possible implementation
myId5 :: a -> b -> b
myId5 _ b = b


functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y


-- this is `id`. returns its argument unchanged
i :: a -> a
i a = a

-- returns its first argument
c :: a -> b -> a
c a b = a

-- this is the same as c above
c'' :: b -> a -> b
c'' = c

-- returns its second argument
c' :: a -> b -> b
c' a b = b

-- returns some part (perhaps all) of its input list
-- e.g. could be tail or id or a permutation
r :: [a] -> [a]
r (x:xs) = xs

-- function composition
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

-- takes function and argument, returns the argument
a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f a = f a
