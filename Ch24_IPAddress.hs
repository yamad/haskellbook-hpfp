-- | Chapter 24, Parser Combinators
-- Exercise 6-9, IP Addesses
module Ch24_IPAddress where

import Control.Applicative ((<|>), Alternative)
import Control.Monad (join)
import Data.Word
import Data.Char (digitToInt)
import Data.Foldable (foldl', toList)
import Data.List (intercalate)
import Numeric (showHex)
import Text.Trifecta
import Text.Parser.LookAhead

import BitUtil

-- | IPv4 address
data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

byte :: Parser Word8
byte = do
  i <- integer
  case validateByte i of
    Nothing -> unexpected "number is out of range (0-255)"
    Just b -> return b

ipv4List :: Parser [Word8]
ipv4List = map fromIntegral <$>
           sepByN 4 byte (char '.') <?> "four dot-separated decimal bytes"

ipv4 :: Parser IPAddress
ipv4 = IPAddress . bytesToTetra <$> ipv4List

ipv4ToWords :: IPAddress -> [Word16]
ipv4ToWords (IPAddress ip) = [hiWyde32 ip, loWyde32 ip]

-- 7. IPv6
data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord)

hex16 :: Parser Word16
hex16 = do
  h <- some hexDigit
  if length h > 4
    then unexpected "too many digits"
    else return (fromIntegral (number 16 h) :: Word16)

fullIPv6List :: Parser [Word16]
fullIPv6List =
  let fullPrefix = sepByN 6 hex16 (char ':') <* char ':'
  in (++) <$> fullPrefix <*> suffixIPv6List

suffixIPv6List :: Parser [Word16]
suffixIPv6List =
  let hexSuffix = sepByN 2 hex16 (char ':')
      ipv4Suffix = ipv4ToWords <$> ipv4
  in try ipv4Suffix <|> hexSuffix

abbrevMark = string "::" <?> "double-colon abbreviator (::)"
singleColon = (char ':' <* notFollowedBy (char ':')) <?> "single colon"

abbrevIPv6Prefix :: Parser [Word16]
abbrevIPv6Prefix =
  ([] <$ abbrevMark) <|>
  sepBy hex16 (try singleColon) <* abbrevMark

abbrevIPv6List :: Parser [Word16]
abbrevIPv6List = do
  first <- abbrevIPv6Prefix
  rest <- many (try $ hex16 <* char ':')
  suffix <- (ipv4ToWords <$> try ipv4) <|> sepBy hex16 singleColon
  case validateIPv6Parts first (rest ++ suffix) of
    Nothing -> unexpected "too many groups"
    Just xs -> return xs

validateIPv6Parts :: [Word16] -> [Word16] -> Maybe [Word16]
validateIPv6Parts prefix rest =
  if nmiss < 1
    then Nothing
    else Just $ prefix ++ fill ++ rest
  where nmiss = 8 - (length prefix + length rest)
        fill  = replicate nmiss 0

ipv6List :: Parser [Word16]
ipv6List = try fullIPv6List <|> abbrevIPv6List

ipv6 :: Parser IPAddress6
ipv6 = uncurry IPAddress6 . convert <$> ipv6List
  where
    convert [a, b, c, d, e, f, g, h] =
      (wydesToOcta a b c d, wydesToOcta e f g h)

ipv6ToInteger :: IPAddress6 -> Integer
ipv6ToInteger (IPAddress6 hi lo) =
  (fromIntegral hi) * 2^64 + fromIntegral lo

sepByN :: (Monad m, Alternative m) => Int -> m a -> m sep -> m [a]
sepByN 0 _ _ = return []
sepByN 1 p _ = (:[]) <$> p
sepByN n p sep = do
  x_sep <- p <* sep
  (x_sep :) <$> sepByN (n - 1) p sep

number :: Integer -> String -> Integer
number base = foldl' (\acc a -> acc * base + toInteger (digitToInt a)) 0



-- 8.
instance Show IPAddress where
  show (IPAddress w32) =
    intercalate "." . map show . quadToList . tetraToBytes $ w32

instance Show IPAddress6 where
  show (IPAddress6 hi lo) = intercalate ":" (show64 hi ++ show64 lo)
    where show64 = map (`showHex` "") . quadToList . octaToWydes



-- 9.
ipv4ToIpv6 :: IPAddress -> IPAddress6
ipv4ToIpv6 (IPAddress w32) = IPAddress6 0 (fromIntegral w32)
