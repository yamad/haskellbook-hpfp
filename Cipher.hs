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


-- | Vigenere cipher
vigenere :: Key -> String -> String
vigenere _ [] = []
vigenere [] xs = xs
vigenere ks (' ':xs) = ' ' : vigenere ks xs -- skip spaces
vigenere (k:ks) (x:xs) = alphaShift (shiftVal k) x : vigenere (ks ++ [k]) xs

unVigenere :: Key -> String -> String
unVigenere _ [] = []
unVigenere [] xs = xs
unVigenere ks (' ':xs) = ' ' : unVigenere ks xs -- skip spaces
unVigenere (k:ks) (x:xs) =
    alphaShift (negate . shiftVal $ k) x : unVigenere (ks ++ [k]) xs


caesarInput :: IO ()
caesarInput = do
  putStr "Enter text to encipher: "
  plaintext <- getLine
  putStr "Enter shift value: "
  shift <- getLine
  putStrLn $ caesar (read shift) plaintext

vigenereInput :: IO ()
vigenereInput = do
  putStr "Enter text to encipher: "
  plaintext <- getLine
  putStr "Enter cipher key: "
  key <- getLine
  putStrLn $ vigenere key plaintext

prop_caesarRoundtrip :: ShiftAmount -> String -> Bool
prop_caesarRoundtrip n s = (unCaesar n . caesar n) s == s

prop_vigenereRoundtrip :: Key -> String -> Bool
prop_vigenereRoundtrip k s = (unVigenere k . vigenere k) s == s

testMain :: IO ()
testMain = do
  putStrLn "Caesar cipher ::"
  caesarInput
  putStrLn "Vigenere cipher ::"
  vigenereInput
