module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

killTo :: (Eq a) => a -> [a] -> [a]
killTo a = drop 1 . dropWhile (/= a)

myLines :: String -> [String]
myLines [] = []
myLines xs = takeWhile (/='\n') xs : myLines (killTo '\n' xs)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = takeWhile (/= c) xs : (splitOn c . killTo $ xs)
  where killTo = drop 1 . dropWhile (/= c)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
           ++ show (myLines sentences == shouldEqual)
           ++ " "
           ++ show (splitOn '\n' sentences == shouldEqual)
