module Ch24_DotTests where

import Ch24_Dot
import Control.Applicative (liftA2)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Text.Trifecta (Result(..), parseString)

runDotFile :: FilePath -> IO (Result DotGraph)
runDotFile fpath = parseString graph mempty <$> readFile fpath

runDirectory :: FilePath -> IO [(FilePath, Bool)]
runDirectory fdir =
  let fnames = filter (isExtension ".gv") <$> listDirectory fdir
      testParse = fmap resultTruth . runDotFile . (fdir ++)
  in liftA2 zip fnames (traverse testParse =<< fnames)

isExtension :: String -> FilePath -> Bool
isExtension ext = (ext ==) . takeExtension

resultTruth :: Result DotGraph -> Bool
resultTruth (Success _) = True
resultTruth (Failure _) = False
