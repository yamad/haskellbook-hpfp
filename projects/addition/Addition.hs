module Addition where

import Test.Hspec
import Test.QuickCheck

multiplyIterate :: (Integral a) => a -> a -> a
multiplyIterate m1 m2 = go m1 m2 0
  where go a b acc
          | a == 0    = 0
          | b == 0    = acc
          | b < 0     = go a (b + 1) (acc - a)
          | otherwise = go a (b - 1) (acc + a)


-- | QuickCheck generators

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple
    :: (Arbitrary a, Arbitrary b, Arbitrary c)
    => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater




main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it  "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "multiplyIterate" $ do
    it "is commutative" $ do
      multiplyIterate 2 3 `shouldBe` multiplyIterate 3 2
      multiplyIterate 5 1 `shouldBe` multiplyIterate 1 5
      multiplyIterate 0 1 `shouldBe` multiplyIterate 1 0
    it "Zero is 0" $ do
      multiplyIterate 0 2 `shouldBe` 0
    it "1 is identity" $ do
      multiplyIterate 1 2 `shouldBe` 2
    it "works for negative numbers" $ do
      multiplyIterate (-2) 2 `shouldBe` -4
      multiplyIterate 2 (-2) `shouldBe` -4
