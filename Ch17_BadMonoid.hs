module Ch17_BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

-- need to define EqProp for custom datatypes
instance EqProp Bull where (=-=) = eq

main :: IO ()
-- `monoid` is a TestBatch from checkers library
-- pass a value to tell `monoid` which type to test
-- value itself does not matter, just the type
main = quickBatch (monoid Twoo)
