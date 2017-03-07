module Char8ProllyNotWhatYouWant where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.ByteString.UTF8 as UTF8


-- Manual unicode encoding of Japanese text
-- GHC Haskell allows UTF8 in source files
s :: String
s = "\12371\12435\12395\12385\12399\12289\
\20803\27671\12391\12377\12363\65311"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint = putStrLn . T.unpack . TE.decodeUtf8

throwsException :: IO ()
throwsException = utf8ThenPrint (B8.pack s)

bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO ()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork
