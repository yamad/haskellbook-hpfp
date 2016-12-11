module Ch23_Tests where

import Ch23

main :: IO ()
main = do
  print $ runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
