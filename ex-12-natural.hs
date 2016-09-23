module C12.Natural where

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat z
    | z < 0 = Nothing
    | otherwise = Just $ go z
  where
    go 0 = Zero
    go n = Succ (go (n - 1))
