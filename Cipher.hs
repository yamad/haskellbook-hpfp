module Cipher where

import Data.Char
import Test.QuickCheck

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
vignere [] xs = xs
vignere ks (' ':xs) = ' ' : vignere ks xs -- skip spaces
vignere (k:ks) (x:xs) = alphaShift (shiftVal k) x : vignere (ks ++ [k]) xs

unVignere :: Key -> String -> String
unVignere _ [] = []
unVignere [] xs = xs
unVignere ks (' ':xs) = ' ' : unVignere ks xs -- skip spaces
unVignere (k:ks) (x:xs) =
    alphaShift (negate . shiftVal $ k) x : unVignere (ks ++ [k]) xs


caesarInput :: IO ()
caesarInput = do
  putStr "Enter text to encipher: "
  plaintext <- getLine
  putStr "Enter shift value: "
  shift <- getLine
  putStrLn $ caesar (read shift) plaintext

vignereInput :: IO ()
vignereInput = do
  putStr "Enter text to encipher: "
  plaintext <- getLine
  putStr "Enter cipher key: "
  key <- getLine
  putStrLn $ vignere key plaintext

prop_caesarRoundtrip :: ShiftAmount -> String -> Bool
prop_caesarRoundtrip n s = (unCaesar n . caesar n) s == s

prop_vignereRoundtrip :: Key -> String -> Bool
prop_vignereRoundtrip k s = (unVignere k . vignere k) s == s

main :: IO ()
main = do
  putStrLn "Caesar cipher ::"
  caesarInput
  putStrLn "Vignere cipher ::"
  vignereInput
