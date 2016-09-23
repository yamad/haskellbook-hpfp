import Data.Char (isUpper, toLower)
import Data.List (elemIndex, elemIndices, group, sort, sortOn)

data Button =
    Button Char
           String

data DaPhone =
    DaPhone [Button]

standardPhone :: DaPhone
standardPhone =
    DaPhone
        [ Button '1' "1"
        , Button '2' "abc2"
        , Button '3' "def3"
        , Button '4' "ghi4"
        , Button '5' "jkl5"
        , Button '6' "mno6"
        , Button '7' "pqrs7"
        , Button '8' "tuv8"
        , Button '9' "wxyz9"
        , Button '0' " "
        , Button '#' "."
        ]

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol lol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Haha thanks just making sure rofl ur turn"
    ]

type Digit = Char

type Presses = Int

charTaps :: Button -> Char -> [(Digit, Presses)]
charTaps b@(Button key chars) c =
    case elemIndex c chars of
        Just n -> [(key, n + 1)]
        Nothing -> []

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) c = error "character not found"
reverseTaps p@(DaPhone bs) c
    | isUpper c = ('*', 1) : reverseTaps p (toLower c)
    | otherwise = foldr ((++) . flip charTaps c) [] bs

phoneEncode :: DaPhone -> String -> [(Digit, Presses)]
phoneEncode p = concatMap (reverseTaps p)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = phoneEncode

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

highestFreq
    :: (Ord a)
    => [a] -> a
highestFreq = head . last . sortOn length . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = highestFreq . map toLower

letterCost :: String -> Char -> Presses
letterCost s c = noccur * taps
  where
    noccur = length . elemIndices c $ s
    taps = fingerTaps . reverseTaps standardPhone $ c

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = highestFreq . concatMap words
