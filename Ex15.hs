{-# LANGUAGE CPP #-}
module Ex15.Monoid where

import Data.Semigroup
import Test.QuickCheck
import Test.Hspec

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Optional a) where
    Nada <> o = o
    o <> Nada = o
    (Only o1) <> (Only o2) = Only (o1 <> o2)

instance (Semigroup a) =>
         Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
    mconcat
        [ e
        , "! he said "
        , adv
        , " as he jumped into his car "
        , noun
        , " and drove off with his "
        , adj
        , " wife."
        ]

newtype First' a = First'
    { getFirst' :: Optional a
    } deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) = firstMappend

instance Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' Nada) b = b
firstMappend a _             = a

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

-- | QuickCheck generator for First'
firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
    a <- arbitrary
    frequency [(1, return $ First' Nada), (3, return $ First' (Only a))]

instance Arbitrary a =>
         Arbitrary (First' a) where
    arbitrary = firstGen

-- |  Monoid Laws
-------------------
monoidAssoc
    :: (Eq m, Monoid m)
    => m -> m -> m -> Bool
monoidAssoc a b c = (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftIdentity
    :: (Eq m, Monoid m)
    => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity
    :: (Eq m, Monoid m)
    => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a


data Trivial =
    Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _  _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialId = Trivial -> Bool


newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Identity a) where
    (Identity a) <> (Identity a') = Identity $ a <> a'

instance (Semigroup a, Monoid a) =>
         Monoid (Identity a) where
    mempty = Identity $ mempty
    mappend = (<>)

instance Arbitrary a =>
         Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssoc  = Identity String
                    -> Identity String
                    -> Identity String
                    -> Bool
type IdentityId = Identity String -> Bool


data Two a b =
    Two a
        b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) =>
         Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc = Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool
type TwoId = Two [Int] String -> Bool

data Three a b c =
    Three a
          b
          c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
    (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Semigroup a, Semigroup b, Semigroup c, Monoid a, Monoid b, Monoid c) =>
         Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc = Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Bool
type ThreeId = Three [Int] [Int] [Int] -> Bool


data Four a b c d =
    Four a
         b
         c
         d
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
    (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Semigroup a
         ,Semigroup b
         ,Semigroup c
         ,Semigroup d
         ,Monoid a
         ,Monoid b
         ,Monoid c
         ,Monoid d) =>
         Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourAssoc = Four [Int] [Int] [Int] [Int] -> Four [Int] [Int] [Int] [Int] -> Four [Int] [Int] [Int] [Int] -> Bool
type FourId = Four [Int] [Int] [Int] [Int] -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return $ BoolConj b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> (BoolDisj _) = BoolDisj True
    (BoolDisj _) <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False

instance Monoid BoolDisj where
   mempty = BoolDisj False
   mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return $ BoolDisj b

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool


data Or a b
    = Fst a
    | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst _) <> a@(Fst _) = a
    b@(Snd _) <> _ = b
    _ <> b@(Snd _) = b

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

type OrAssoc = Or [Int] String -> Or [Int] String -> Or [Int] String -> Bool

newtype Combine a b = Combine
    { unCombine :: (a -> b)
    }

instance (Semigroup b) =>
         Semigroup (Combine a b) where
    f <> g = Combine $ \a -> unCombine f a <> unCombine g a

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ mempty
    mappend = (<>)

instance CoArbitrary a =>
         CoArbitrary (Combine a b) where
    coarbitrary = coarbitrary

--instance Show (Combine a b) where
--   show = show . unCombine

type CombineAssoc = Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool

combineAssoc
    :: (Semigroup b, Eq b)
    => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc a b c d = (unCombine f $ d) == (unCombine g $ d)
  where
    f = (a <> (b <> c))
    g = ((a <> b) <> c)


newtype Comp a = Comp
    { unComp :: (a -> a)
    }

instance Semigroup (Comp a) where
    f <> g = Comp $ unComp f . unComp g

instance Monoid (Comp a) where
    mempty = Comp $ id
    mappend = (<>)


data Validation a b
    = Failure' a
    | Success' b
    deriving (Eq, Show)

-- collect failures, take first Success
instance Semigroup a =>
         Semigroup (Validation a b) where
    (Failure' a) <> (Failure' a') = Failure' (a <> a')
    v@(Failure' _) <> _ = v
    _ <> v@(Failure' _) = v
    v@(Success' _) <> _ = v

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure' a, Success' b]

type ValidAssoc  = Validation [Int] [Int]
                -> Validation [Int] [Int]
                -> Validation [Int] [Int]
                -> Bool

-- collect success, return first failure
newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b =>
         Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' b)) <> (AccumulateRight (Success' b')) =
        AccumulateRight (Success' (b <> b'))
    v@(AccumulateRight (Failure' _)) <> _ = v
    _ <> v@(AccumulateRight (Failure' _)) = v

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [ AccumulateRight $ Failure' a
                 , AccumulateRight $ Success' b ]

type AccumulateRightAssoc = AccumulateRight [Int] [Int] -> AccumulateRight [Int] [Int] -> AccumulateRight [Int] [Int] -> Bool

-- collect both successes and failures
newtype AccumulateBoth a b =
    AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success' a)) <> (AccumulateBoth (Success' a')) =
        AccumulateBoth (Success' (a <> a'))
    (AccumulateBoth (Failure' b)) <> (AccumulateBoth (Failure' b')) =
        AccumulateBoth (Failure' (b <> b'))
    v@(AccumulateBoth (Failure' _)) <> _ = v
    _ <> v@(AccumulateBoth (Failure' _)) = v

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [ AccumulateBoth $ Failure' a
                 , AccumulateBoth $ Success' b ]

type AccumulateBothAssoc = AccumulateBoth [Int] [Int] -> AccumulateBoth [Int] [Int] -> AccumulateBoth [Int] [Int] -> Bool


newtype Mem s a = Mem
    { runMem :: s -> (a, s)
    }

instance Semigroup a =>
         Semigroup (Mem s a) where
    f <> g =
        Mem $
        \s ->
             let (a', s') = runMem f s
                 (a'', s'') = runMem g s'
             in (a' <> a'', s'')

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == (runMem f' 0 :: (String, Int))
    print $ runMem (mempty <> f') 0 == runMem f' 0
    hspec $
        describe "First'" $
        do it "has right identity" $
               ((First' (Only 1) <> First' Nada) :: First' Int) `shouldBe`
               First' (Only 1)
           it "has left identity" $
               ((First' Nada <> First' (Only 2)) :: First' Int) `shouldBe`
               First' (Only 2)
           it "always gives first" $
               (First' (Only 1) <> First' (Only 2) :: First' Int) `shouldBe`
               First' (Only 1)
           it "gives empty if both are empty" $
               ((First' Nada <> First' Nada) :: First' Int) `shouldBe` First' Nada
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: TrivialId)
    quickCheck (monoidRightIdentity :: TrivialId)
    quickCheck (semigroupAssoc :: FirstMappend)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: IdentityId)
    quickCheck (monoidRightIdentity :: IdentityId)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: TwoId)
    quickCheck (monoidRightIdentity :: TwoId)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (monoidLeftIdentity :: ThreeId)
    quickCheck (monoidRightIdentity :: ThreeId)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (monoidLeftIdentity :: FourId)
    quickCheck (monoidRightIdentity :: FourId)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConjId)
    quickCheck (monoidRightIdentity :: BoolConjId)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisjId)
    quickCheck (monoidRightIdentity :: BoolDisjId)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (semigroupAssoc :: ValidAssoc)
    quickCheck (semigroupAssoc :: AccumulateRightAssoc)
    quickCheck (semigroupAssoc :: AccumulateBothAssoc)
--    quickCheck (combineAssoc :: CombineAssoc)
