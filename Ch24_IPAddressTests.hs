module Ch24_IPAddressTests where

import Ch24_IPAddress

import Text.Trifecta
import Test.Hspec

import BitUtil

runParser :: Parser a -> String -> Maybe a
runParser p = maybeSuccess . parseString p mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main =
  hspec $
  do describe "IPv6 List" $
       do it "accepts full representation" $
            do runParser ipv6List "0000:0000:0000:0000:0000:ffff:ac10:fe01" `shouldBe`
                 Just [0, 0, 0, 0, 0, 0xffff, 0xac10, 0xfe01]
               runParser ipv6List "0000:0000:0000:0000:0000:ffff:cc78:f" `shouldBe`
                 Just [0, 0, 0, 0, 0, 0xffff, 0xcc78, 0xf]
               runParser ipv6List "2001:0db8:85a3:0000:0000:8a2e:0370:7334" `shouldBe`
                 Just [0x2001, 0x0db8, 0x85a3, 0x0, 0x0, 0x8a2e, 0x0370, 0x7334]
          it "accepts omitted leading zeros" $
            do runParser ipv6List "0:0:0:0:0:ffff:ac10:fe01" `shouldBe`
                 Just [0, 0, 0, 0, 0, 0xffff, 0xac10, 0xfe01]
               runParser ipv6List "0:0:0:0:0:ffff:cc78:f" `shouldBe`
                 Just [0, 0, 0, 0, 0, 0xffff, 0xcc78, 0xf]
               runParser ipv6List "2001:db8:85a3:0:0:8a2e:370:7334" `shouldBe`
                 Just [0x2001, 0x0db8, 0x85a3, 0x0, 0x0, 0x8a2e, 0x0370, 0x7334]
          it "accepts middle double-colon (::) abbreviations" $
            do runParser ipv6List "2001:db8:85a3::8a2e:370:7334" `shouldBe`
                 Just [0x2001, 0x0db8, 0x85a3, 0x0, 0x0, 0x8a2e, 0x0370, 0x7334]
               runParser ipv6List "2001:db8::8:800:200c:417a" `shouldBe`
                 Just [0x2001, 0xdb8, 0x0, 0x0, 0x8, 0x800, 0x200c, 0x417a]
               runParser ipv6List "1::8" `shouldBe`
                 Just [1, 0, 0, 0, 0, 0, 0, 8]
          it "accepts leading double-colon (::) abbreviations" $
            do runParser ipv6List "::1" `shouldBe` Just [0, 0, 0, 0, 0, 0, 0, 1]
               runParser ipv6List "::2:3:4:5:6:7:8" `shouldBe`
                 Just [0, 2, 3, 4, 5, 6, 7, 8]
          it "accepts trailing double-colon (::) abbreviations" $
            do runParser ipv6List "1::" `shouldBe` Just [1, 0, 0, 0, 0, 0, 0, 0]
               runParser ipv6List "1:2:3:4::" `shouldBe`
                 Just [1, 2, 3, 4, 0, 0, 0, 0]
          it "double-colon must indicate one or more groups (not zero)" $
            do runParser ipv6List "::1:2:3:4:5:6:7:8" `shouldBe` Nothing
               runParser ipv6List "1::2:3:4:5:6:7:8" `shouldBe` Nothing
          it "requires 8 groups" $ do runParser ipv6List "1:2:3:4" `shouldBe` Nothing
          it "accepts IPv4 address as final 4 bytes" $
            do runParser ipv6List "1:2:3:4:5:6:1.2.3.4" `shouldBe`
                 Just [0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x102, 0x304]
          it "accepts IPv4 address with leading double-colon" $
            do runParser ipv6List "::1.2.3.4" `shouldBe`
                 Just [0, 0, 0, 0, 0, 0, 0x102, 0x304]
          it "accepts IPv4 address, with preceding double-colon" $
            do runParser ipv6List "1::1.2.3.4" `shouldBe`
                 Just [0x1, 0, 0, 0, 0, 0, 0x102, 0x304]
          it "accepts IPv4 address, with central colon" $
            do runParser ipv6List "1::6:1.2.3.4" `shouldBe`
                 Just [0x1, 0, 0, 0, 0, 0x6, 0x102, 0x304]
     describe "IPv6 parser" $
       do it "accepts full length input with low octabyte only" $
            do runParser ipv6 "0:0:0:0:0:ffff:ac10:fe01" `shouldBe`
                 Just (IPAddress6 0 281473568538113)
               runParser ipv6 "0:0:0:0:0:ffff:cc78:f" `shouldBe`
                 Just (IPAddress6 0 281474112159759)
          it "accepts input to high and lo octabytes" $
            do ipv6ToInteger <$>
                 runParser ipv6 "2001:db8::8:800:200c:417a" `shouldBe`
                 Just 42540766411282592856906245548098208122
               ipv6ToInteger <$>
                 runParser ipv6 "fe80::0202:b3ff:fe1e:8329" `shouldBe`
                 Just 338288524927261089654163772891438416681
