module Cipher where

import Data.Char

type ShiftAmount = Int
type Key = String

-- | take letter to new letter by rotating shift (A -> B, Z -> A)
--
-- handles uppercase and lowercase letters. pass through for all other
-- characters
alphaShift :: ShiftAmount -> Char -> Char
alphaShift shift c
  | isAlpha c = chr shiftedVal
  | otherwise = c
  where base = if isUpper c then 'A' else 'a'
        rotatedVal = ((ord c - ord base) + shift) `mod` 26
        shiftedVal = rotatedVal + ord base

-- | Caesar cipher
caesar :: ShiftAmount -> String -> String
caesar shift = map (alphaShift shift)

-- | Reverse Caesar cipher
unCaesar :: ShiftAmount -> String -> String
unCaesar shift = map (alphaShift (negate shift))


shiftVal :: Char -> Int
shiftVal c = ord c - ord base
  where base = if isUpper c then 'A' else 'a'


-- | Vignere cipher
vignere :: Key -> String -> String
vignere _ [] = []
vignere ks (' ':xs) = ' ' : vignere ks xs -- skip spaces
vignere (k:ks) (x:xs) = alphaShift (shiftVal k) x : vignere (ks ++ [k]) xs
