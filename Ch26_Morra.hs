{-# LANGUAGE OverloadedStrings #-}
-- | Chapter 26, Monad Transformers, Morra Game
module Ch26_Morra where

import Control.Monad (liftM2)
import Control.Monad.Trans.State
import Data.List (sortOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

data Game = Game [Player]
  deriving (Eq, Show)

data Player = Player
  { plName :: String
  , plScore :: Integer
  , plType :: PlayerType
  }
  deriving Eq

data PlayerType
  = Human
  | Computer
  deriving (Eq, Show)

instance Show Player where
  show p =
    concat
      ["Player: ", plName p, "(" ++ show (plType p) ++ ",", show (plScore p) ++ ")"]

type Turn = (FingerCount, GuessSum)
type FingerCount = Integer
type GuessSum = Integer

type Morra m = StateT Game m [Player]
type MorraIO = Morra IO

data MorraError
  = FingersOutOfRange
  | InputNotInteger

-- | return list of changes to player scores
winners :: [Turn] -> [Bool]
winners ts = map (isWin fingerTotal) ts
  where
    fingerTotal = sum $ map fst ts
    isWin :: FingerCount -> Turn -> Bool
    isWin total (_, guess) = guess == total

updateScore :: Player -> Bool -> Player
updateScore pl isWin =
  if isWin
    then pl
         { plScore = plScore pl + 1
         }
    else pl

updateGame :: Monad m => [Turn] -> Morra m
updateGame t =
  StateT $
  \(Game ps) ->
     let awards = winners t
         playerAwards = zip ps awards
         ps' = fmap (uncurry updateScore) playerAwards
         playerWins = fst <$> filter snd (zip ps' awards)
     in return (playerWins, Game $ ps')

winThreshold :: Integer
winThreshold = 3

runGame :: MorraIO
runGame =
  StateT $
  \g -> do
    (wins, g') <- runStateT runRound g
    case filter isWinner wins of
      [] -> runStateT runGame g' -- no winners yet
      ws -> do                   -- winner(s) found, report and exit
        reportScores g'
        reportGameWins ws
        return (wins, g')

isWinner :: Player -> Bool
isWinner p = plScore p >= winThreshold

reportScores :: Game -> IO ()
reportScores (Game ps) = do
  putStrLn "- Final Scores:"
  mapM_ (putStrLn . scoreLine) (reverse . sortOn plScore $ ps)
  where
    scoreLine p = concat ["-   ", plName p, ": ", show $ plScore p]

reportGameWins :: [Player] -> IO ()
reportGameWins pWinners = do
  mapM_ (\p -> putStrLn $ "- " ++ plName p ++ " wins game!") pWinners

reportRoundWins :: [Player] -> IO ()
reportRoundWins = mapM_ (\p -> putStrLn ("- " ++ plName p ++ " gets point"))

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

mkHuman :: String -> Player
mkHuman name = Player name 0 Human

mkComputer :: String -> Player
mkComputer name = Player name 0 Computer

getName :: IO String
getName = putStr "Enter name: " >> getLine

getTurns :: Game -> IO [Turn]
getTurns (Game players) = traverse (getTurn nplayers) players
  where nplayers = fromIntegral . length $ players

getTurn :: Integer -> Player -> IO Turn
getTurn nplayers (Player name _ typ) = do
  putStr $ "Player " ++ name
  case typ of
    Human -> do
      putStrLn ", enter turn"
      getHumanTurn
    Computer -> do
      putStrLn " turn generated"
      genRandomTurn nplayers

-- | generate random turn, given a count of players
genRandomTurn :: Integer -> IO Turn
genRandomTurn nplayers =
  (,) <$> randomRIO (0, 5) <*> randomRIO (0, 5 * nplayers)

-- Human input
---------------
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


main :: IO ()
main = do
  g <- makeGame
  _ <- runStateT runGame g
  return ()
