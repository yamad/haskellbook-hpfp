module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where
    stream = iterate (+ 1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: Int -> M.Map Int Int
insertMap k = M.insert k 10001 m

insertSet :: Int -> S.Set Int
insertSet k = S.insert k s

partitionMap :: Int -> (M.Map Int Int, M.Map Int Int)
partitionMap i = M.partition (>i) m

partitionSet :: Int -> (S.Set Int, S.Set Int)
partitionSet i = S.partition (>i) s

mapMap :: Int -> M.Map Int Int
mapMap i = M.mapKeys (+i) m

mapSet :: Int -> S.Set Int
mapSet i = S.map (+i) s

main :: IO ()
main =
  defaultMain
    [ bench "map - member check" $ whnf membersMap 9999
    , bench "set - member check" $ whnf membersSet 9999
    , bench "map - insert check" $ whnf insertMap 10001
    , bench "set - insert check" $ whnf insertSet 10001
    , bench "map - partition check" $ whnf partitionMap 100
    , bench "set - partition check" $ whnf partitionSet 100
    , bench "map - map check" $ whnf mapMap 100
    , bench "set - map check" $ whnf mapSet 100
    ]
