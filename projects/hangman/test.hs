module Main where

import Hangman
import Test.QuickCheck
import Test.Hspec

puzzleGen :: Gen Puzzle
puzzleGen = do
  s <- arbitrary
  -- TODO: do some guesses first
  return $ freshPuzzle s

puzzleNewGen :: Gen Puzzle
puzzleNewGen = do
  a <- arbitrary
  return $ freshPuzzle a

instance Arbitrary Puzzle where
  arbitrary = puzzleNewGen

prop_constantSecret :: Puzzle -> Char -> Bool
prop_constantSecret p@(Puzzle w _ _) c = w == w'
  where (Puzzle w' _ _ ) = fillInCharacter p c

prop_addGuessedChar :: Puzzle -> Char -> Bool
prop_addGuessedChar p c = elem c g'
  where
    (Puzzle _ _ g') = fillInCharacter p c

prop_noRepeatGuess :: Puzzle -> Char -> Bool
prop_noRepeatGuess p@(Puzzle _ _ g) c =
    if elem c g
        then g == g'
        else length g' == length g + 1
  where
    (Puzzle _ _ g') = fillInCharacter p c

main :: IO ()
main =
    hspec $
    do describe "fillInCharacter" $
           do it "does not alter secret string" $ do property $ prop_constantSecret
              it "adds guessed character" $ do property $ prop_addGuessedChar
              it "don't repeat guessed characters" $ do property $ prop_noRepeatGuess
              it "correct guesses fill in puzzle" $
                  do let (Puzzle _ fill _) =
                             fillInCharacter (freshPuzzle "abc") 'a'
                     fill `shouldBe` [Just 'a', Nothing, Nothing]
              it "incorrect guesses leave puzzle in original state" $
                  do let p = freshPuzzle "abc"
                     let (Puzzle _ fill _) = p
                     let (Puzzle _ fill' _) = fillInCharacter p 'd'
                     fill `shouldBe` fill'
       describe "handleGuess" $
           do it "returns same puzzle on repeat guess" $
                  do let p = freshPuzzle "abc"
                     let p' = fillInCharacter p 'a'
                     p'' <- handleGuess p' 'a'
                     p'' `shouldBe` p'
              it "correct guess fills puzzle" $ do
                 let p = freshPuzzle "abc"
                 p'@(Puzzle _ fill _) <- handleGuess p 'a'
                 [Just 'a', Nothing, Nothing] `shouldBe` fill
