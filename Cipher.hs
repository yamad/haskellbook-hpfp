module Cipher where

import Data.Char

-- | take letter to new letter by rotating shift (A -> B, Z -> A)
--
-- handles uppercase and lowercase letters. pass through for all other
-- characters
alphaShift :: Int -> Char -> Char
alphaShift shift c
  | isAlpha c = chr shiftedVal
  | otherwise = c
  where base = if isUpper c then 'A' else 'a'
        rotatedVal = ((ord c - ord base) + shift) `mod` 26
        shiftedVal = rotatedVal + ord base

caesar :: Int -> String -> String
caesar shift = map (alphaShift shift)

unCaesar :: Int -> String -> String
unCaesar shift = map (alphaShift (negate shift))
