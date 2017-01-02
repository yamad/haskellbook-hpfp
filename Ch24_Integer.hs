-- | Chapter 24, Parser Combinators, Chapter Exercises
-- Exercises 2 and 3, Integer parsers
module Ch24_Integer where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Text.Trifecta

-- 2.
parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . map (toInteger . digitToInt)
  where listToInteger :: [Integer] -> Integer
        listToInteger = foldl1 (\acc a -> acc * 10 + a)

base10Integer :: Parser Integer
base10Integer = digitsToInteger <$> some parseDigit


-- 3.
base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional (char '+' <|> char '-')
  num <- base10Integer
  case sign of
    Just '-' -> return $ negate num
    _        -> return num
