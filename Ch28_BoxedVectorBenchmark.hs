module Main where

import Criterion.Main
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

bv :: BV.Vector Int
bv = {-# SCC "Boxed" #-} BV.fromList [1 .. 10000]

uv :: UV.Vector Int
uv = {-# SCC "Unboxed" #-} UV.fromList [1 .. 10000]

main :: IO ()
main =
  defaultMain
    [ bench "boxed vector" $ whnf (BV.head . BV.slice 100 900) bv
    , bench "unboxed vector" $ whnf (UV.head . UV.slice 100 900) uv
    ]
