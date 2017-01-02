{-# LANGUAGE QuasiQuotes #-}
-- | Chapter 24, Parser Combinators
-- Exercise 5. Log file parser
module Ch24_LogFile where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import Data.List (intercalate, sort)
import Data.Tuple (swap)
import Text.Trifecta
import Text.RawString.QQ

data Log
  = Log [DailyLog]

instance Show Log where
  show (Log dailies) = concatMap show dailies

data DailyLog = DailyLog
  { dlDate :: Date
  , dlAgenda :: Agenda
  }

instance Show DailyLog where
  show (DailyLog date agenda) = unlines ["# " ++ show date, show agenda]

data Agenda
  = Agenda [AgendaItem] (Map.Map Activity [DiffTime])

instance Show Agenda where
  show (Agenda items _) = unlines . map show $ items

data AgendaItem = AgendaItem
  { getTime :: Time
  , getActivity :: Activity
  }

instance Show AgendaItem where
  show (AgendaItem t a) = show t ++ " " ++ a

makeAgenda :: [AgendaItem] -> Agenda
makeAgenda items = Agenda items agendaMap
  where
    agendaMap = Map.fromListWith (++) $ zip (map getActivity items) dTimes
    dTimes = map ((:[]) . uncurry makeDiffTime) $ zip times (tail times)
    times = map getTime items

tallyAgendaTime :: Agenda -> [(Activity, Int)]
tallyAgendaTime (Agenda _ mAD) =
  sort . Map.toList . Map.map (sum . map getDiffTime) $ mAD

averageAgendaTime :: Agenda -> [(Activity, Double)]
averageAgendaTime (Agenda _ mAD) =
  sort . Map.toList . Map.map (average . map getDiffTime) $ mAD
  where
    average xs = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

tallyLog :: Log -> [(Activity, Double)]
tallyLog (Log dailies) =  sort $ concatMap (averageAgendaTime . dlAgenda) dailies

data Date = Date
  { year :: Int
  , month :: Int
  , day :: Int
  }
  deriving (Eq)

instance Show Date where
  show (Date y m d) = intercalate "-" [show y, pad m, pad d]

data Time = Time
  { hour :: Int
  , minute :: Int
  }
  deriving (Eq)

instance Show Time where
  show (Time h m) = pad h ++ ":" ++ pad m

pad :: Int -> String
pad a
  | a < 10    = '0' : show a
  | otherwise = show a

-- | Difference between two Times in minutes
newtype DiffTime = DiffTime
  { getDiffTime :: Int
  } deriving (Eq, Show)

makeDiffTime :: Time -> Time -> DiffTime
makeDiffTime t1 t2 = DiffTime $ (toMinutes t2) - (toMinutes t1)
  where toMinutes (Time h m) = h * 60 + m

type Activity = String


-- Parsing ----

commentMark :: String
commentMark = "--"

dateMark :: String
dateMark = "#"

parseDate :: Parser Date
parseDate = do
  yearString <- count 4 digit
  _ <- char '-'
  monthString <- count 2 digit
  _ <- char '-'
  dayString <- count 2 digit
  return $
    Date
      (fromInteger . digitsToInteger $ yearString)
      (fromInteger . digitsToInteger $ monthString)
      (fromInteger . digitsToInteger $ dayString)

parseTime :: Parser Time
parseTime = do
  hourString <- count 2 digit
  _ <- char ':'
  minString <- count 2 digit
  return $
    Time
      (fromInteger . digitsToInteger $ hourString)
      (fromInteger . digitsToInteger $ minString)

comment :: (Monad m, CharParsing m) => m ()
comment = do
  _ <- string commentMark
  skipMany (notChar '\n') <?> "comment"

eol :: Parser ()
eol = do
  _ <- oneOf "\r\n"
  return () <?> "end of line"

commentSpace :: (Monad m, TokenParsing m) => m ()
commentSpace = do
  _ <- spaces
  _ <- skipMany $ token comment <?> "comments and whitespace"
  return ()

tokenC :: (Monad m, TokenParsing m) => m a -> m a
tokenC p = p <* (commentSpace <|> pure ())

parseAgendaItem :: Parser AgendaItem
parseAgendaItem = do
  time <- tokenC parseTime
  activity <- manyTill anyChar (try eol <|> try comment <|> eof)
  return $ AgendaItem time activity

parseAgenda :: Parser Agenda
parseAgenda = do
  items <- many $ tokenC parseAgendaItem <?> "list of agenda items"
  return $ makeAgenda items

parseDailyLog :: Parser DailyLog
parseDailyLog = do
  --skipOptional commentSpace
  date <- symbol dateMark *> tokenC parseDate
  agenda <- parseAgenda
  return $ DailyLog date agenda

parseLog :: Parser Log
parseLog = do
  skipOptional commentSpace <?> "parseLog skip"
  dailies <- some (tokenC parseDailyLog) <?> "all daily logs"
  return $ Log dailies

digitsToInteger :: String -> Integer
digitsToInteger = listToInteger . map (toInteger . digitToInt)
  where listToInteger :: [Integer] -> Integer
        listToInteger = foldl1 (\acc a -> acc * 10 + a)

testLog :: String
testLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
