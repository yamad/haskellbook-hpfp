module BitUtil where

import Control.Applicative (liftA2)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word

validateByte :: Integer -> Maybe Word8
validateByte b
  | b < 0 = Nothing
  | b > 255 = Nothing
  | otherwise = Just (fromIntegral b :: Word8)

toWyde :: (Word8, Word8) -> Word16
toWyde (hi, lo) =
  let hi16 = fromIntegral hi :: Word16
      lo16 = fromIntegral lo :: Word16
  in shiftL hi16 8 .|. lo16

bytesToTetra :: [Word8] -> Word32
bytesToTetra bytes =
  let [hi, mhi, mlo, lo] = map fromIntegral bytes :: [Word32]
  in sum [shiftL hi 24, shiftL mhi 16, shiftL mlo 8, shiftL lo 0]

wydesToOcta :: Word16 -> Word16 -> Word16 -> Word16 -> Word64
wydesToOcta hi mhi mlo lo =
  let [hi', mhi', mlo', lo'] = map fromIntegral [hi, mhi, mlo, lo] :: [Word64]
  in sum
       [ shiftL hi' $ 16 * 3
       , shiftL mhi' $ 16 * 2
       , shiftL mlo' $ 16 * 1
       , shiftL lo' $ 16 * 0
       ]


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, a') = (f a, f a')

mapQuad :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapQuad f (a, b, c, d) = (f a, f b, f c, f d)

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

quadToList :: (a, a, a, a) -> [a]
quadToList (a, b, c, d) = [a, b, c, d]

tuplesToQuad :: (a, a) -> (a, a) -> (a, a, a, a)
tuplesToQuad (a, b) (c, d) = (a, b, c, d)



hiByte16 :: Word16 -> Word8
hiByte16 = fromIntegral . flip shiftR 8

loByte16 :: Word16 -> Word8
loByte16 = fromIntegral

hiWyde32 :: Word32 -> Word16
hiWyde32 = fromIntegral . flip shiftR 16

loWyde32 :: Word32 -> Word16
-- type conversion truncates high bytes
loWyde32 = fromIntegral -- . flip shiftR 0

hiTetra64 :: Word64 -> Word32
hiTetra64 = fromIntegral . flip shiftR 32

loTetra64 :: Word64 -> Word32
loTetra64 = fromIntegral



octaToTetras :: Word64 -> (Word32, Word32)
octaToTetras = liftA2 (,) hiTetra64 loTetra64

tetraToWydes :: Word32 -> (Word16, Word16)
tetraToWydes = liftA2 (,) hiWyde32 loWyde32

octaToWydes :: Word64 -> (Word16, Word16, Word16, Word16)
octaToWydes = uncurry tuplesToQuad . mapTuple tetraToWydes . octaToTetras

wydeToBytes :: Word16 -> (Word8, Word8)
wydeToBytes = liftA2 (,) hiByte16 loByte16

tetraToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
tetraToBytes = uncurry tuplesToQuad . mapTuple wydeToBytes . tetraToWydes
