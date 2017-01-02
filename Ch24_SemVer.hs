{-# LANGUAGE FlexibleInstances #-}
-- | Chapter 24, Parser Combinators
-- Chapter Exercise 1, Semantic Versioning
-- http://semver.org
module Ch24_SemVer where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Text.Trifecta

-- | a Semantic Version
data SemVer =
  SemVer Major
         Minor
         Patch
         Release
         Metadata
  deriving (Eq, Show)

-- | Release/metadata info can be any sequence of numbers or strings
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

-- strings are before numbers
instance Ord NumberOrString where
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS s) (NOSS t) = compare s t
  compare (NOSI a) (NOSI b) = compare a b

instance {-# OVERLAPPING #-} Ord [NumberOrString] where
  -- pre-releases are lower precedence
  -- than normal version (with no
  -- pre-release information)
  compare [] [] = EQ
  compare [] (_:_) = GT
  compare (_:_) [] = LT
  -- otherwise, compare all elements
  -- (longer lists have higher precedence)
  compare xs ys =
    mconcat (uncurry compare <$> zip xs ys)
    `mappend` compare (length xs) (length ys)

-- Precedence (see section 11) for semantic versions
instance Ord SemVer where
  compare (SemVer major minor patch rel _) (SemVer major' minor' patch' rel' _) =
    mconcat
      [ compare major major'
      , compare minor minor'
      , compare patch patch'
      , compare rel rel'
      -- metadata is ignored for precedence calculation
      ]

-- | Parse a Semantic Version number
parseSemVer :: Parser SemVer
parseSemVer = do
  -- major.minor.patch format is required
  major <- noLeadingZero
  _     <- char '.'
  minor <- noLeadingZero
  _     <- char '.'
  patch <- noLeadingZero
  -- release and metadata dot-separated sequences are optional
  rel   <- option [] parseRelease
  meta  <- option [] parseMeta
  return (SemVer major minor patch rel meta)

-- | Identifiers include only [A-Za-z0-9]. Cannot be empty. Cannot be
-- all digits
identifier :: (Monad m, CharParsing m) => m String
identifier = do
  a <- many digit <?> "possible leading numbers"
  b <- some (letter <|> char '_') <?> "must have at least one non-digit"
  c <- many (letter <|> digit <|> char '_') <?> "alphanumeric or underscore"
  return $ a ++ b ++ c

noLeadingZero :: Parser Integer
noLeadingZero = do
  c <- loneZero <|> nonZeroStart
  return $ digitsToInteger c
  where
    loneZero = string "0" <* notFollowedBy digit
    nonZeroStart = do
      a <- oneOf "123456789"
      b <- many digit
      return $ a : b

digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . map (toInteger . digitToInt)
  where listToInteger :: [Integer] -> Integer
        listToInteger = foldl1 (\acc a -> acc * 10 + a)

-- | Parse a number or string, parameterized by integer parser
parseNOS :: Parser Integer -> Parser NumberOrString
parseNOS pI =
  try (NOSI <$> pI <* notFollowedBy letter) <|> (NOSS <$> identifier)

-- | Parse release version (see section 9)
--
-- String should be composed of dot-separated identifiers following a
-- hyphen. Numbers cannot have leading zeros (see sec. 9).
parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  sepBy1 (parseNOS noLeadingZero) (char '.') <?> "release string"

-- | Parse build metadata (see section 10)
--
-- String should be composed of dot-separated identifiers following a
-- '+'. Unlike release data, numbers may have leading zeros.
parseMeta :: Parser Metadata
parseMeta = do
  _ <- char '+'
  sepBy1 (parseNOS integer) (char '.') <?> "metadata string"
