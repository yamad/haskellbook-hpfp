-- | Chapter 24, Parser Combinators.
-- Exercise 1. Semantic Versioning Parser
module Ch24_SemVerTests where

import Ch24_SemVer

import Text.Trifecta
import Test.Hspec

runSVParse :: String -> Maybe SemVer
runSVParse = maybeSuccess . parseString parseSemVer mempty

runParser :: Parser a -> String -> Maybe a
runParser p = maybeSuccess . parseString p mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main =
  hspec $
  do describe "noLeadingZero parser" $
       do it "accepts single zero as digit" $
            do runParser noLeadingZero "0" `shouldBe` Just 0
          it "rejects string of only zeros" $
            do runParser noLeadingZero "00" `shouldBe` Nothing
          it "rejects single leading zero" $
            do runParser noLeadingZero "01" `shouldBe` Nothing
          it "rejects >1 leading zeros" $
            do runParser noLeadingZero "001" `shouldBe` Nothing
          it "accepts all non-zero digits" $
            do runParser noLeadingZero "1" `shouldBe` Just 1
               runParser noLeadingZero "2" `shouldBe` Just 2
               runParser noLeadingZero "3" `shouldBe` Just 3
               runParser noLeadingZero "4" `shouldBe` Just 4
               runParser noLeadingZero "5" `shouldBe` Just 5
               runParser noLeadingZero "6" `shouldBe` Just 6
               runParser noLeadingZero "7" `shouldBe` Just 7
               runParser noLeadingZero "8" `shouldBe` Just 8
               runParser noLeadingZero "9" `shouldBe` Just 9
          it "accepts non-leading zeros" $
            do runParser noLeadingZero "10" `shouldBe` Just 10
               runParser noLeadingZero "100" `shouldBe` Just 100
               runParser noLeadingZero "101" `shouldBe` Just 101
          it "extracts whole numbers from string with non-number suffix" $
            do runParser noLeadingZero "0a" `shouldBe` Just 0
               runParser noLeadingZero "0 " `shouldBe` Just 0
               runParser noLeadingZero "1 " `shouldBe` Just 1
               runParser noLeadingZero "12 " `shouldBe` Just 12
     describe "SemVer Normal Version Number (Section 2)" $
       do it "accepts X.Y.Z format" $
            do runSVParse "1.9.0" `shouldBe` Just (SemVer 1 9 0 [] [])
               runSVParse "1.10.0" `shouldBe` Just (SemVer 1 10 0 [] [])
          it "accepts 0 as major, minor, or patch" $
            do runSVParse "0.0.0" `shouldBe` Just (SemVer 0 0 0 [] [])
          it "rejects leading zero for major version" $
            do runSVParse "01.0.0" `shouldBe` Nothing
          it "rejects leading zero for minor version" $
            do runSVParse "0.01.0" `shouldBe` Nothing
          it "rejects leading zero for patch version" $
            do runSVParse "0.0.01" `shouldBe` Nothing
          it "rejects negative numbers" $ do runSVParse "-1.0.0" `shouldBe` Nothing
     describe "SemVer Pre-release Version (Section 9)" $
       do it "accepts string identifiers" $
            do runSVParse "1.0.0-alpha" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "alpha"] [])
          it "accepts numeric identifiers" $
            do runSVParse "1.0.0-0.3.7" `shouldBe`
                 Just (SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] [])
          it "aceepts a mix of string and numeric identifiers" $
            do runSVParse "1.0.0-alpha.1" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
               runSVParse "1.0.0-x.7.z.92" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
          it "accepts alphanumeric identifiers as strings" $
            do runSVParse "1.0.0-0123abc" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "0123abc"] [])
               runSVParse "1.0.0-123abc" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "123abc"] [])
               runSVParse "1.0.0-a1b2" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "a1b2"] [])
          it "rejects empty identifiers" $
            do runSVParse "1.0.0-a..b" `shouldBe` Nothing
          it "rejects numbers with leading zeros" $
            do runSVParse "1.0.0-01" `shouldBe` Nothing
     describe "SemVer Metadata Version (Section 10)" $
       do it "accepts string identifiers" $
            do runSVParse "1.0.0+exp.sha.5114f85" `shouldBe`
                 Just (SemVer 1 0 0 [] [NOSS "exp", NOSS "sha", NOSS "5114f85"])
          it "accepts numeric identifiers" $
            do runSVParse "1.0.0+20130313144700" `shouldBe`
                 Just (SemVer 1 0 0 [] [NOSI 20130313144700])
          it "accepts a mix of string and numeric identifiers" $
            do runSVParse "1.0.0+exp.1.sha.2" `shouldBe`
                 Just (SemVer 1 0 0 [] [NOSS "exp", NOSI 1, NOSS "sha", NOSI 2])
          it "accepts identifiers with leading zeros" $
            do runSVParse "1.0.0+001" `shouldBe` Just (SemVer 1 0 0 [] [NOSI 1])
          it "accepts optional releases before metadata" $
            do runSVParse "1.0.0-alpha+001" `shouldBe`
                 Just (SemVer 1 0 0 [NOSS "alpha"] [NOSI 1])
          it "rejects empty identifiers" $
            do runSVParse "1.0.0+exp..sha" `shouldBe` Nothing
     describe "SemVer Precedence (Section 11)" $
       do it "orders major versions numerically" $
            do let v1 = Just (SemVer 1 0 0 [] [])
                   v2 = Just (SemVer 2 0 0 [] [])
               compare v1 v2 `shouldBe` LT
               compare v2 v1 `shouldBe` GT
               compare v1 v1 `shouldBe` EQ
          it "orders minor versions numerically" $
            do let v200 = Just (SemVer 2 0 0 [] [])
                   v210 = Just (SemVer 2 1 0 [] [])
               compare v200 v210 `shouldBe` LT
               compare v210 v210 `shouldBe` EQ
          it "orders patch versions numerically" $
            do let v210 = Just (SemVer 2 1 0 [] [])
                   v211 = Just (SemVer 2 1 1 [] [])
               compare v210 v211 `shouldBe` LT
               compare v211 v211 `shouldBe` EQ
          it "orders release versions lexicographically" $
            do let alpha = Just (SemVer 1 0 0 [NOSS "alpha"] [])
                   beta = Just (SemVer 1 0 0 [NOSS "beta"] [])
               compare alpha beta `shouldBe` LT
          it "orders release versions numerically" $
            do let i1 = Just (SemVer 1 0 0 [NOSI 1] [])
                   i2 = Just (SemVer 1 0 0 [NOSI 2] [])
               compare i1 i2 `shouldBe` LT
          it "orders release numbers lower than strings" $
            do let a1 = Just (SemVer 1 0 0 [NOSI 1] [])
                   ab = Just (SemVer 1 0 0 [NOSS "beta"] [])
               compare a1 ab `shouldBe` LT
          it "orders release numbers lower length is lower" $
            do let a = Just (SemVer 1 0 0 [NOSS "alpha"] [])
                   a1 = Just (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
                   ab = Just (SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] [])
                   b = Just (SemVer 1 0 0 [NOSS "beta"] [])
                   b2 = Just (SemVer 1 0 0 [NOSS "beta", NOSI 2] [])
                   b11 = Just (SemVer 1 0 0 [NOSS "beta", NOSI 11] [])
                   rc1 = Just (SemVer 1 0 0 [NOSS "rc", NOSI 1] [])
               compare a a1 `shouldBe` LT
               compare a ab `shouldBe` LT
               compare a1 ab `shouldBe` LT
               compare ab b `shouldBe` LT
               compare b b2 `shouldBe` LT
               compare b2 b11 `shouldBe` LT
               compare b11 rc1 `shouldBe` LT
          it "major versions take precedence over minor versions" $
            do let v110 = Just (SemVer 1 1 0 [] [])
                   v200 = Just (SemVer 2 0 0 [] [])
               compare v110 v200 `shouldBe` LT
               compare v200 v110 `shouldBe` GT
          it "major versions take precedence over patch versions" $
            do let v101 = Just (SemVer 1 0 1 [] [])
                   v200 = Just (SemVer 2 0 0 [] [])
               compare v101 v200 `shouldBe` LT
               compare v200 v101 `shouldBe` GT
          it "minor versions take precedence over patch versions" $
            do let v101 = Just (SemVer 1 0 1 [] [])
                   v110 = Just (SemVer 1 1 0 [] [])
               compare v101 v110 `shouldBe` LT
               compare v110 v101 `shouldBe` GT
          it "major versions take precedence over release versions" $
            do let v1a = Just (SemVer 1 0 0 [NOSS "alpha"] [])
                   v2a = Just (SemVer 2 0 0 [NOSS "alpha"] [])
               compare v1a v2a `shouldBe` LT
          it "minor versions take precedence over release versions" $
            do let v01a = Just (SemVer 0 1 0 [NOSS "alpha"] [])
                   v02a = Just (SemVer 0 2 0 [NOSS "alpha"] [])
               compare v01a v02a `shouldBe` LT
          it "patch versions take precedence over release versions" $
            do let v001a = Just (SemVer 0 0 1 [NOSS "alpha"] [])
                   v002a = Just (SemVer 0 0 2 [NOSS "alpha"] [])
               compare v001a v002a `shouldBe` LT
          it "normal versions take precedence over pre-release versions" $
            do let v1 = Just (SemVer 1 0 0 [] [])
                   v1a = Just (SemVer 1 0 0 [NOSS "alpha"] [])
               compare v1a v1 `shouldBe` LT
          it "build metadata does not affect precedence" $
            do let v1a1 = Just (SemVer 1 0 0 [NOSS "alpha"] [NOSI 1])
                   v1a2 = Just (SemVer 1 0 0 [NOSS "alpha"] [NOSI 2])
               compare v1a1 v1a2 `shouldBe` EQ
