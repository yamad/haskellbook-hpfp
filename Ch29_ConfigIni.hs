module Ch29_ConfigIni where

import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as M
import Data.List (isSuffixOf)
import System.Directory (listDirectory)

import Ch24_DataIni (Config, parseIniContents)

iniMap :: [FilePath] -> IO (M.Map FilePath (Maybe Config))
iniMap fs = do
  values <- mapM (fmap parseIniContents . BS.readFile) fs
  return $ M.fromList (zip fs values)

dirIniMap :: FilePath -> IO (M.Map FilePath (Maybe Config))
dirIniMap dir = (filter (isSuffixOf "ini") <$> listDirectory dir) >>= iniMap
