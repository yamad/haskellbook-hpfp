module Hangman where

import Control.Monad (forever)
import Data.Maybe (isJust, catMaybes)
import Data.List (intersperse, elem)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
    WordList [String]
    deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
  where
    gameLength w =
        let l = length (w :: String)
        in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- | A Puzzle holds the secret string, discovered characters in order,
-- and guessed characters
data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far : " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle secret _ _) c = elem c secret

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
        if wordChar == guessed
            then Just wordChar
            else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn
                "You already guessed that\
               \ character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn
                "This character was in the word, \
               \ filling in the word accordingly."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn
                "This character wasn't in\
                   \ the word, try again."
            return (fillInCharacter puzzle guess)

discoveredSecret :: Puzzle -> Bool
discoveredSecret (Puzzle _ filledInSoFar _) = all isJust filledInSoFar

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ filled guesses) =
    length . filter not . fmap (\g -> elem g correct) $ guesses
  where
    correct = catMaybes filled

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filled guessed) =
    if ((incorrectGuesses p) > 7 && (not . discoveredSecret $ p))
        then do
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
        else return ()

gameWin :: Puzzle -> IO ()
gameWin p =
    if discoveredSecret p
        then do
            putStrLn "You win!"
            exitSuccess
        else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                    \ be a single character"
