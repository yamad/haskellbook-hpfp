module Ex13 where

import Control.Monad (forever)
import Data.Char (isAlphaNum, toLower)
import System.Exit (exitSuccess)

-- | examine only letters and numbers, ignoring
isPalindrome :: String -> Bool
isPalindrome s = str1 == reverse str1
  where
    str1 = filter isAlphaNum . fmap toLower $ s

palindrome :: IO ()
palindrome =
    forever $
    do line1 <- getLine
       case isPalindrome line1 of
           True -> putStrLn "It's a palindrome!"
           False -> do
               putStrLn "Nope!"
               exitSuccess

type Name = String

type Age = Integer

data Person =
    Person Name
           Age
    deriving (Show)

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq)

instance Show PersonInvalid where
    show NameEmpty = "Name was empty"
    show AgeTooLow = "Age was too low"
    show (PersonInvalidUnknown s) = s

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $
        PersonInvalidUnknown $
        "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    age <- getLine
    case mkPerson name (read age) of
        Left err -> putStrLn $ "An error occurred: " ++ show err
        Right p -> print p
    return ()
