-- | Chapter 24, Parser Combinators
-- Exercise 4, Phone Number (NANP scheme) parser
module Ch24_Phone where

import Data.Char (digitToInt)
import Text.Trifecta

type NumberingPlanArea = Int    -- area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber  =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = choice [parseDashed, parseTraditional]

-- format:   XXX-XXX-XXXX
--         X-XXX-XXX-XXXX
--           XXXXXXXXXX
-- dashes are optional, so can be present or absent in any combination
parseDashed :: Parser PhoneNumber
parseDashed = do
  skipOptional (try $ char '1' >> char '-')
  npa <- count 3 digit
  skipOptional $ char '-'
  exc <- count 3 digit
  skipOptional $ char '-'
  lno <- count 4 digit
  return $
    PhoneNumber
      (fromInteger . digitsToInteger $ npa)
      (fromInteger . digitsToInteger $ exc)
      (fromInteger . digitsToInteger $ lno)

-- format: (XXX) XXX-XXXX
parseTraditional :: Parser PhoneNumber
parseTraditional = do
  npa <- parens (count 3 digit)
  exc <- count 3 digit
  _ <- char '-'
  lno <- count 4 digit
  return $
    PhoneNumber
      (fromInteger . digitsToInteger $ npa)
      (fromInteger . digitsToInteger $ exc)
      (fromInteger . digitsToInteger $ lno)


digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . map (toInteger . digitToInt)
  where listToInteger :: [Integer] -> Integer
        listToInteger = foldl1 (\acc a -> acc * 10 + a)
