module C12.Maybe where

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' _ = True

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def _ Nothing = def
mayybee _ f (Just a) = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' def = mayybee def id

listToMaybe' :: [a] -> Maybe a
listToMaybe' []    = Nothing
listToMaybe' (x:_) = Just x

maybeToList' :: Maybe a -> [a]
maybeToList' = mayybee [] (:[])

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr ((++) . maybeToList') []

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' = foldr go (Just [])
  where
    go _ Nothing = Nothing
    go Nothing _ = Nothing
    go (Just a) (Just b) = Just (a : b)
