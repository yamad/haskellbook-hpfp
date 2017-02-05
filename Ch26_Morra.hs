{-# LANGUAGE OverloadedStrings #-}

-- | Chapter 26, Monad Transformers, Morra Game
module Ch26_Morra where

import Control.Monad.Trans.State
import Control.Monad (void)
import Data.Foldable (foldl')
import Data.List (delete, isPrefixOf, sortOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

------------------------------
-- * Constants
------------------------------

winThreshold :: Integer
winThreshold = 3

-------------------------------
-- * Types
-------------------------------

data Game =
  Game [Player]
  deriving (Eq, Show)

data Player = Player
  { plName :: String
  , plScore :: Integer
  , plType :: PlayerType
  , plHistory :: [FingerCount]
  } deriving (Eq)

data PlayerType
  = Human
  | Computer
  deriving (Eq, Show)

type Turn = (FingerCount, GuessSum)

type FingerCount = Integer

type GuessSum = Integer

type Morra m = StateT Game m [Player]

type MorraIO = Morra IO

data MorraError
  = FingersOutOfRange
  | InputNotInteger

instance Show Player where
  show (Player name score typ hist) =
    concat
      ["Player: ", name, "(" ++ show typ ++ ",", show score ++ ")", show hist]


-------------------------------
-- * Game Engine
-------------------------------

-- | return list of changes to player scores
winners :: [Turn] -> [Bool]
winners ts = map (isWin fingerTotal) ts
  where
    fingerTotal = sum $ map fst ts
    isWin :: FingerCount -> Turn -> Bool
    isWin total (_, guess) = guess == total

-- | Save turn information to given player
updateTurn :: Player -> Turn -> Player
updateTurn pl (count, _) =
  pl
  { plHistory = count : plHistory pl
  }

-- | Update player score if they won a point
updateScore :: Player -> Bool -> Player
updateScore pl isWin =
  if isWin
    then pl
         { plScore = plScore pl + 1
         }
    else pl

updateGame
  :: Monad m
  => [Turn] -> Morra m
updateGame turns =
  StateT $
  \(Game ps) ->
     let awards = winners turns
         ps' = zipWith updateScore ps awards
         ps'' = zipWith updateTurn ps' turns
         playerWins = fst <$> filter snd (zip ps'' awards)
     in return (playerWins, Game ps'')

isWinner :: Player -> Bool
isWinner p = plScore p >= winThreshold

mkHuman :: String -> Player
mkHuman name = Player name 0 Human []

mkComputer :: String -> Player
mkComputer name = Player name 0 Computer []


-------------------------------
-- * Input
-------------------------------

getPlayers :: IO [Player]
getPlayers = do
  putStr "Enter # of players: "
  nplayers <- getInteger
  putStrLn ""
  case nplayers of
    Left _ -> do
      putStrLn "Must enter a number"
      getPlayers
    Right n ->
      traverse
        (\i -> do
           putStrLn $ "Player " ++ show i
           getPlayer)
        [1 .. n]

getPlayer :: IO Player
getPlayer = do
  putStr "Choose (H)uman/(C)omputer: "
  typeChoice <- getLine
  if typeChoice `elem` ["0", "H"]
    then mkHuman <$> getName
    else mkComputer <$> getName

getHumanTurn :: IO Turn
getHumanTurn = (,) <$> getFingerCount <*> getGuess

getFingerCount :: IO FingerCount
getFingerCount = do
  putStr "Fingers (0-5): "
  efc <- validateFingerCount <$> getInteger
  case efc of
    Left _ -> do
      putStrLn "Invalid input"
      getFingerCount
    Right fc -> return fc

getGuess :: IO GuessSum
getGuess = do
  putStr "Guess: "
  eg <- getInteger
  case eg of
    Left _ -> do
      putStrLn "Invalid input"
      getGuess
    Right g -> return g

getName :: IO String
getName = putStr "Enter name: " >> getLine

getTurns :: Game -> IO [Turn]
getTurns (Game players) = traverse (getTurn players) players

getTurn :: [Player] -> Player -> IO Turn
getTurn ps p@(Player name _ typ _) = do
  putStr $ "Player " ++ name
  case typ of
    Human -> do
      putStrLn ", enter turn"
      getHumanTurn
    Computer -> do
      putStrLn " turn generated"
      genPredictTurn (delete p ps)

-- | generate random turn, given list of all players
genRandomTurn :: [Player] -> IO Turn
genRandomTurn ps = do
  fc <- randomRIO (0, 5)
  guess <- randomRIO (0, 5 * fromIntegral (length ps - 1))
  return (fc, fc + guess)

getInteger :: IO (Either [MorraError] Integer)
getInteger = do
  input <- fmap readMaybe getLine
  case input of
    Nothing -> return $ Left [InputNotInteger]
    Just a -> return $ Right a

validateFingerCount :: Either [MorraError] Integer -> Either [MorraError] Integer
validateFingerCount (Left err) = Left err
validateFingerCount (Right i)
  | i >= 0 && i <= 5 = Right i
  | otherwise = Left [FingersOutOfRange]



---------------------------
-- * AI Prediction, 3-grams
---------------------------

-- | Generate turn by prediction, given list of all players
genPredictTurn :: [Player] -> IO Turn
genPredictTurn ps = do
   fc <- randomRIO (0, 5)
   guess <- sum <$> traverse predictOrGuess ps
   return (fc, fc + guess)

-- | Generate prediction or guess, as appropriate, for a player
--
-- Predicts for human players when the last two moves have already
-- been seen. If last two moves have been seen more than once, pick
-- randomly from all known moves.
--
-- Randomly guesses when there is not enough information for a
-- prediction or it is a computer player (whose productions are
-- random).
predictOrGuess :: Player -> IO FingerCount
predictOrGuess pl =
  case plType pl of
    Computer -> randomRIO (0, 5)
    Human ->
      case predictNgram 3 $ plHistory pl of
        -- randomly select from previous guesses
        Just xs -> (xs !!) <$> randomRIO (0, length xs - 1)
        -- or fallback to random guess
        Nothing -> randomRIO (0, 5)

-- | Return known completions for current N-gram of length
-- `n`. Returns @Nothing@ if no completions are known.
--
-- e.g. @predictNgram 3 [1,2,3,1,2,5]@ looks for 3-grams that complete
-- the head 3-gram @(2,1,x)@. Note reverse order of input list.  The
-- 3-gram (2,1,3) occurs deeper in the list, so the result is @Just
-- [3]@.
predictNgram :: Integer -> [Integer] -> Maybe [Integer]
predictNgram n xs = do
  pre <- takeMaybe (fromIntegral (n-1)) xs
  case followedWith pre xs of
    [] -> Nothing
    xxs -> Just xxs

-- | return values from second list that precede the sequence in the
-- first list. That is,
--
--   @followedWith [1,2] [3,1,2] == [3]@
--   @followedWith [1] [2,1,3,1,4] == [2,3]@
followedWith :: [Integer] -> [Integer] -> [Integer]
followedWith _ [] = []
followedWith pre (x:xs)
  | pre `isPrefixOf` xs = x : rest
  | otherwise = rest
  where
    rest = followedWith pre xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _  = Just []
takeMaybe _ [] = Nothing
takeMaybe n (x:xs) = (x:) <$> takeMaybe (n - 1) xs



--------------------
-- * Printing/Report
--------------------

reportScores :: Game -> IO ()
reportScores (Game ps) = do
  putStrLn "- Final Scores:"
  mapM_ (putStrLn . scoreLine) (reverse . sortOn plScore $ ps)
  where
    scoreLine p =
      concat ["-   ", plName p, ": ", show $ plScore p]

reportGameWins :: [Player] -> IO ()
reportGameWins = mapM_ (\p -> putStrLn $ concat ["- ", plName p, " wins game!"])

reportRoundWins :: [Player] -> IO ()
reportRoundWins = mapM_ (\p -> putStrLn ("- " ++ plName p ++ " gets point"))



-------------------------------
-- * Game Constructors
-------------------------------

runGame :: MorraIO
runGame =
  StateT $
  \g -> do
    (wins, g') <- runStateT runRound g
    case filter isWinner wins of
      [] -> runStateT runGame g' -- no winners yet
      ws -- winner(s) found, report and exit
       -> do
        reportScores g'
        reportGameWins ws
        return (wins, g')

runRound :: MorraIO
runRound =
  StateT $
  \g -> do
    turns <- getTurns g
    (wins, g') <- runStateT (updateGame turns) g
    reportRoundWins wins
    return (wins, g')

makeGame :: IO Game
makeGame = Game <$> getPlayers

main :: IO ()
main = void (makeGame >>= runStateT runGame)
