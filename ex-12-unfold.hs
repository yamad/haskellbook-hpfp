module C12.Unfold where

-- | direct recursion iterate
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- | direct recursion unfoldr
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))



data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f a =
    case f a of
        Nothing -> Leaf
        Just (la, b, ra) -> Node (unfoldTree f la) b (unfoldTree f ra)

-- | build tree with root 0, and n layers
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfoldTree
        (\k ->
              if k < n
                  then Just (k + 1, k, k + 1)
                  else Nothing)
        0
