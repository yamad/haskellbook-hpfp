-- | Chapter 24, Parser Combinators
-- Exercises 2 and 3. Integer parsers
module Ch24_IntegerTests where

import Ch24_Integer

import Control.Applicative (liftA2)
import Text.Trifecta
import Test.Hspec
import qualified Test.QuickCheck as QC

runParser :: Parser a -> String -> Maybe a
runParser p = maybeSuccess . parseString p mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

newtype IntegerString = IntegerString
  { getIntegerString :: String
  }

instance QC.Arbitrary IntegerString where
  arbitrary = QC.sized arbIntegerString

arbIntegerString :: Int -> QC.Gen IntegerString
arbIntegerString 0 = return $ IntegerString ""
arbIntegerString n =
  IntegerString <$>
  liftA2
    (:)
    (QC.elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
    (getIntegerString <$> (arbIntegerString (n - 1)))

main :: IO ()
main =
  hspec $
  do describe "parseDigit parser" $
       do it "returns single digit" $
            do runParser parseDigit "0" `shouldBe` Just '0'
               runParser parseDigit "1" `shouldBe` Just '1'
               runParser parseDigit "2" `shouldBe` Just '2'
               runParser parseDigit "3" `shouldBe` Just '3'
               runParser parseDigit "4" `shouldBe` Just '4'
               runParser parseDigit "5" `shouldBe` Just '5'
               runParser parseDigit "6" `shouldBe` Just '6'
               runParser parseDigit "7" `shouldBe` Just '7'
               runParser parseDigit "8" `shouldBe` Just '8'
               runParser parseDigit "9" `shouldBe` Just '9'
          it "rejects non-digits" $ do runParser parseDigit "abc" `shouldBe` Nothing
     describe "base10Integer parser" $
       do it "accepts leading integer" $
            runParser base10Integer "123abc" `shouldBe` Just 123
          it "accepts leading zeros" $
            runParser base10Integer "0123" `shouldBe` Just 123
          it "rejects non-integers" $
            runParser base10Integer "abc" `shouldBe` Nothing
     describe "base10Integer' parser" $
       do it "accepts unsigned integers" $
            runParser base10Integer' "123" `shouldBe` Just 123
          it "accepts positive integers" $
            runParser base10Integer' "+123" `shouldBe` Just 123
          it "accepts negative integers" $
            runParser base10Integer' "-123" `shouldBe` Just (-123)
