{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module LearnParsers where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"


--- Exercises: Parsing Practice

-- 1.
oneEOF = one >> eof
oneTwoEOF = oneTwo >> eof

-- 2.
-- is there a way to do this without `choice`?
oneMaybeTwoThree :: Parser String
oneMaybeTwoThree = choice [string "123", string "12", string "1"] <* eof

oneMaybeTwoThree' :: Parser String
oneMaybeTwoThree' =
  (++) <$> string "1" <*>
  (option "" ((++) <$> string "2" <*> (option "" (string "3"))))


-- 3.
stringViaChar :: String -> Parser String
stringViaChar s = traverse char s


--- Exercise: Unit of Success
integerOnly :: Parser Integer
integerOnly = integer <* eof


--- Exercise: Try Try
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator can't be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  whole <- decimal
  point <- optional $ char '.'
  case point of
    Nothing -> return $ fromInteger whole
    Just _  -> do
      part <- parseDecimalPart
      return $ fromInteger whole + part

parseDecimalPart :: Parser Double
parseDecimalPart = do
  parts <- decimal
  let denom = 10 ^ integerLen parts
  return (fromInteger parts / fromInteger denom)

listToInt :: [Integer] -> Integer
listToInt = foldl1 (\acc a -> acc * 10 + a)

intToList :: Integer -> [Integer]
intToList = go []
  where go l 0 = l
        go l n = go (n `mod` 10 : l) (n `div` 10)

integerLen :: Integer -> Integer
integerLen = toInteger . length . intToList

type FractionOrDecimal = Either Rational Double

parseFracOrDec :: Parser FractionOrDecimal
parseFracOrDec = (Left <$> try parseFraction) <|> (Right <$> try parseDecimal)
