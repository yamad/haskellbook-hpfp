{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Ex16.Functor where

import GHC.Arr

import Test.QuickCheck
import Test.QuickCheck.Function
import Text.Show.Functions

-- Wait, how does that even typecheck?
--
-- Composing (fmap . fmap)
-- fmap  :: Functor f => (m -> n) -> f m -> f n
-- fmap' :: Functor g => (x -> y) -> g x -> g y
--   (.) :: (b -> c) -> (a -> b) -> a -> c
-- (. fmap')
--
--   (.) :: (b -> c) -> (a -> b) -> a -> c
--          [3]         [  1   ]   [2]
--
-- Applying the second argument to (.) sets `a` and `b`, filling spot [1]
--
-- Due to right associativity of parens with (->), fmap' is:
--
-- fmap' :: Functor g => (x -> y) -> g x -> g y
-- fmap' :: Functor g => (x -> y) -> (g x -> g y)
--                       [   a  ]    [     b    ]
--
-- so,
-- (. fmap') :: Functor g => ((g x -> g y) -> c) -> (x -> y) -> c
-- Now, (fmap . fmap') means applying the first argument [1] of
-- (. fmap') to fmap. What's left is [2], of type (x -> y) -> c.
--
-- (. fmap') :: Functor g => ((g x -> g y) -> c) -> ((x -> y) -> c)
--                           [         1       ]    [       2     ]
--                           [     A     ]   [B]
--
-- fmap  :: Functor f => (m -> n) -> (f m -> f n)
--                       [          1           ]
--                       [   A  ]    [     B    ]
--
-- Thus,
--   (m -> n) == (g x -> g y)
--   (f m -> f n) == c
--   m == g x
--   n == g y
--   c == (f (g x) -> f (g y))
-- so,
-- (fmap . fmap') :: (Functor f, Functor g) =>
--                   (x -> y) -> f (g x) -> f (g y)
-- Exercises: Heavy Lifting
-- a = (+1) $ read "[1]" :: [Int]
aa :: [Int]
aa = fmap (+ 1) $ read "[1]"

-- b = (++ "lol") (Just ["Hi", "Hello"])
bb :: Maybe [String]
bb = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
cc :: Integer -> Integer
cc = (* 2) . (\x -> x - 2)

-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
dd :: Integer -> String
dd = (return '1' ++) . show . (\x -> [x,1 .. 3])

-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
ee :: IO Integer
ee =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
  in fmap (* 3) changed

-- QuickCheck properties
functorIdentity
  :: (Functor f, Eq (f a))
  => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose
  :: (Eq (f c), Functor f)
  => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose'
  :: (Eq (f c), Functor f)
  => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercises: Instances of Func
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- QuickCheck instances
instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IntToInt = Fun Int Int

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a =>
         Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) =>
         Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ LolNope), (3, return $ Yeppers a)]

type PossiblyFC = Possibly Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

type SumFC = Sum Int Int -> IntToInt -> IntToInt -> Bool

-- Can the datatype have an instance of Functor?
-- data Bool = False | True
-- No, is kind *, and needs kind * -> *
data BoolAndSomethingElse a
  = False' a
  | True' a

-- Yes, is kind * -> *
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- Yes, is kind * -> *
newtype Mu f = InF
  { outF :: f (Mu f)
  }

-- No, kind is (* -> *) -> *. Setting the argument f gives kind *.
data D =
  D (Array Word Word)
    Int
    Int

-- No, kind is * (is a type constant)
data Sum' a b
  = Second' a
  | First' b
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' (f a)
  fmap _ (Second' b) = Second' b

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Sum' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First' b, Second' a]

type Sum'FC = Sum' Int Int -> IntToInt -> IntToInt -> Bool

data Company a b c
  = DeepBlue a
             b
  | Something c
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Company a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [DeepBlue a b, Something c]

type CompanyFC = Company Int Int Int -> IntToInt -> IntToInt -> Bool

data More b a
  = L a
      b
      a
  | R b
      a
      b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (More b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [L a b a, R b a b]

type MoreFC = More Int Int -> IntToInt -> IntToInt -> Bool

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ (Finance) = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]

type QuantFC = Quant Int Int -> IntToInt -> IntToInt -> Bool

data K a b =
  K b
  deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K b) = K $ f b

instance (Arbitrary b) =>
         Arbitrary (K a b) where
  arbitrary = do
    b <- arbitrary
    return $ K b

type KFC = K Int Int -> IntToInt -> IntToInt -> Bool

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a
  deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

instance (Arbitrary a) =>
         Arbitrary (K' a b) where
  arbitrary = do
    a <- arbitrary
    return $ K' a

instance (Arbitrary b) =>
         Arbitrary (Flip K' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K' a)

type FlipK'FC = Flip K' Int Int -> IntToInt -> IntToInt -> Bool

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance Arbitrary b =>
         Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

type EvilGoateeConstFC = EvilGoateeConst Int Int -> IntToInt -> IntToInt -> Bool

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

instance Arbitrary a =>
         Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    a <- arbitrary
    elements [LiftItOut (Just a), LiftItOut Nothing]

type LiftItOutFC = LiftItOut Maybe Int -> IntToInt -> IntToInt -> Bool

data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance Arbitrary a =>
         Arbitrary (Parappa [] [] a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ DaWrappa [a] [a']

type ParappaFC = Parappa [] [] Int -> IntToInt -> IntToInt -> Bool

data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)
  deriving (Eq, Show)

instance (Functor g) =>
         Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (IgnoreOne [] [] a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ IgnoringSomething [a] [b]

type IgnoreOneFC = IgnoreOne [] [] Int Int -> IntToInt -> IntToInt -> Bool

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Eq, Show)

instance Functor g =>
         Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary o, Arbitrary a, Arbitrary t) =>
         Arbitrary (Notorious [] o a t) where
  arbitrary = do
    o <- arbitrary
    a <- arbitrary
    t <- arbitrary
    return $ Notorious [o] [a] [t]

type NotoriousFC = Notorious [] Int Int Int -> IntToInt -> IntToInt -> Bool


data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = sized arbList

arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 = return Nil
arbList n =
  frequency [(1, return Nil), (4, Cons <$> arbitrary <*> (arbList (n - 1)))]


type ListFC = List Int -> IntToInt -> IntToInt -> Bool


data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

instance Arbitrary a =>
         Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    frequency
      [ (1, return NoGoat)
      , (2, return $ OneGoat a)
      , (2, return $ MoreGoats (OneGoat b) (OneGoat c) (OneGoat d))
      ]

type GoatLordFC = GoatLord Int -> IntToInt -> IntToInt -> Bool


data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)
  deriving (Show)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read $ f . g


main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: PairFC)
  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck (functorCompose' :: Three'FC)
  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck (functorCompose' :: FourFC)
  quickCheck $ \x -> functorIdentity (x :: Four' Int Int)
  quickCheck (functorCompose' :: Four'FC)
  quickCheck $ \x -> functorIdentity (x :: Possibly Int)
  quickCheck (functorCompose' :: PossiblyFC)
  quickCheck $ \x -> functorIdentity (x :: Sum Int Int)
  quickCheck (functorCompose' :: SumFC)
  quickCheck $ \x -> functorIdentity (x :: Sum' Int Int)
  quickCheck (functorCompose' :: Sum'FC)
  quickCheck $ \x -> functorIdentity (x :: Company Int Int Int)
  quickCheck (functorCompose' :: CompanyFC)
  quickCheck $ \x -> functorIdentity (x :: More Int Int)
  quickCheck (functorCompose' :: MoreFC)
  quickCheck $ \x -> functorIdentity (x :: Quant Int Int)
  quickCheck (functorCompose' :: QuantFC)
  quickCheck $ \x -> functorIdentity (x :: K Int Int)
  quickCheck (functorCompose' :: KFC)
  quickCheck $ \x -> functorIdentity (x :: Flip K' Int Int)
  quickCheck (functorCompose' :: FlipK'FC)
  quickCheck $ \x -> functorIdentity (x :: EvilGoateeConst Int Int)
  quickCheck (functorCompose' :: EvilGoateeConstFC)
  quickCheck $ \x -> functorIdentity (x :: LiftItOut Maybe Int)
  quickCheck (functorCompose' :: LiftItOutFC)
  quickCheck $ \x -> functorIdentity (x :: Parappa [] [] Int)
  quickCheck (functorCompose' :: ParappaFC)
  quickCheck $ \x -> functorIdentity (x :: IgnoreOne [] [] Int Int)
  quickCheck (functorCompose' :: IgnoreOneFC)
  quickCheck $ \x -> functorIdentity (x :: Notorious [] Int Int Int)
  quickCheck (functorCompose' :: NotoriousFC)
  quickCheck $ \x -> functorIdentity (x :: List Int)
  quickCheck (functorCompose' :: ListFC)
  quickCheck $ \x -> functorIdentity (x :: GoatLord Int)
  quickCheck (functorCompose' :: GoatLordFC)
