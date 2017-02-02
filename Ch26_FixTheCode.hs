-- | Chapter 26, Monad Transformers, Fix the Code Exercise (pg. 1021)
module Ch26_FixTheCode where

import Control.Monad
import Control.Monad.IO.Class    -- << added for liftIO
import Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine             -- << added liftIO
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite -- << added runMaybeT
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
