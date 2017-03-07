module Main where

import Criterion.Main
import qualified Data.Sequence as S

listOfLists :: [[Int]]
listOfLists = replicate 10 [1..100000]

seqOfSeqs :: [S.Seq Int]
seqOfSeqs = replicate 10 (S.fromList [1 .. 100000])

lists :: [Int]
lists = [1..100000]

seqs :: S.Seq Int
seqs = S.fromList [1..100000]

main :: IO ()
main =
  defaultMain
    [ bench "concatenate lists" $ nf mconcat listOfLists
    , bench "concatenate sequences" $ nf mconcat seqOfSeqs
    , bench "indexing list" $ whnf (\xs -> xs !! 9001) lists
    , bench "indexing sequence" $ whnf (flip S.index 9001) seqs
    ]
