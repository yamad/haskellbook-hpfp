module C12.String where

import Data.Maybe (fromJust)
import Data.Char (toLower)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromJust . nothingToA . notThe) . words
  where nothingToA Nothing = Just "a"
        nothingToA s       = s

isVowel :: Char -> Bool
isVowel = flip elem "aeiou" . toLower

isVowelInitial :: String -> Bool
isVowelInitial ""    = False
isVowelInitial (x:_) = isVowel x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go :: [String] -> Integer
    go []  = 0
    go [_] = 0
    go ("the":xs) =
        if isVowelInitial (head xs)
            then 1 + go xs
            else 0 + go xs
    go (_:xs) = go xs

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

countConsonants :: String -> Integer
countConsonants s = (toInteger . length) s - countVowels s

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
    if countVowels s <= countConsonants s
        then Just (Word' s)
        else Nothing
