{-# LANGUAGE OverloadedStrings #-}
module Ch26_ExercisesTests where

import Ch26_Exercises
import Test.Hspec

import Control.Monad.Trans.Reader

main :: IO ()
main = hspec $ do
  describe "rDec" $ do
    it "decrements value by 1" $
      runReader rDec 1 `shouldBe` 0
    it "maps over functors (e.g. lists)" $
      fmap (runReader rDec) [1..10] `shouldBe` [0..9]
  describe "rShow" $ do
    it "shows input, wrapped in Reader" $
      runReader rShow 1 `shouldBe` "1"
    it "maps over functors" $
      fmap (runReader rShow) [1..3] `shouldBe` ["1", "2", "3"]
