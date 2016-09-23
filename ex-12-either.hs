module C12.Either where

lefts' :: [Either a b] -> [a]
lefts' =
    foldr
        (\x acc ->
              case x of
                  Left a -> a : acc
                  _ -> acc)
        []

rights' :: [Either a b] -> [b]
rights' =
    foldr
        (\x acc ->
              case x of
                  Right b -> b : acc
                  _ -> acc)
        []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
    foldr
        (\x (ls, rs) ->
              case x of
                  Left l -> (l : ls, rs)
                  Right r -> (ls, r : rs))
        ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
