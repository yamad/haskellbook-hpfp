addExclamation :: String -> String
addExclamation = (++ "!")

fourthLetter :: String -> String
fourthLetter x = [x !! 4]

drop9 :: String -> String
drop9 = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

crry :: String
crry = "Curry is awesome"

letterIndex :: Int -> Char
letterIndex x = crry !! x

rvrs :: String
rvrs = drop 9 crry ++ take 4 (drop 5 crry) ++ take 5 crry

main :: IO ()
main = do
  print $ addExclamation "Curry is awesome"
  print $ fourthLetter   "Curry is awesome"
  print $ drop9          "Curry is awesome"
  print $ thirdLetter    "Curry is awesome"
