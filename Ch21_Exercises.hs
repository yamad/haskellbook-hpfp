-- | Chapter 21, Traversable, Chapter Exercises
module Ch21_Exercises where


-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a


-- Constant
newtype Constant a b = Constant
  { getConstant :: a
  }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a


-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a


-- List
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons (f a) (fmap f r)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a r) = f a `mappend` foldMap f r

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a r) = Cons <$> f a <*> traverse f r


-- Three
data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c


-- Three'
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b `mappend` f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'


-- S
data S n a =
  S (n a)
    a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na `mappend` f a

instance Traversable n =>
         Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a


-- Tree
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node r a l) = Node (fmap f r) (f a) (fmap f l)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node r a l) = foldMap f r `mappend` f a `mappend` foldMap f l

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node r a l) = Node <$> traverse f r <*> f a <*> traverse f l
