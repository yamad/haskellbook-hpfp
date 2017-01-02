module Ch24_PhoneTests where

import Ch24_Phone

import Text.Trifecta
import Test.Hspec

runParser :: Parser a -> String -> Maybe a
runParser p = maybeSuccess . parseString p mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main =
  hspec $
  do describe "Phone number parser" $
       do it "parses XXX-XXX-XXXX" $
            runParser parsePhone "123-456-7890" `shouldBe`
            Just (PhoneNumber 123 456 7890)
          it "parses X-XXX-XXX-XXXX" $
            runParser parsePhone "1-123-456-7890" `shouldBe`
            Just (PhoneNumber 123 456 7890)
          it "parses XXXXXXXXXX" $
            runParser parsePhone "1234567890" `shouldBe`
            Just (PhoneNumber 123 456 7890)
          it "parses (XXX) XXX-XXXX" $
            runParser parsePhone "(123) 456-7890" `shouldBe`
            Just (PhoneNumber 123 456 7890)
