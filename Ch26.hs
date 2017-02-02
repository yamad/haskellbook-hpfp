-- | Chapter 26, Monad transformers
module Ch26 where

import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

--- MaybeT
newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance (Functor m) =>
         Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) =>
         Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) =>
         Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $
    do v <- ma
       case v of
         Nothing -> return Nothing
         Just y -> runMaybeT (f y)

--- Exercises: EitherT
newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

-- 1.
instance Functor m =>
         Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2.
instance Applicative m =>
         Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT mea) = EitherT $ liftA2 (<*>) fab mea

-- 3.
instance Monad m =>
         Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $
    do v <- mea
       case v of
         Left e -> return $ Left e
         Right a -> runEitherT (f a)

-- 4.
swapEitherT
  :: Functor m
  => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where
    swapEither (Left a) = Right a
    swapEither (Right a) = Left a

-- 5.
eitherT
  :: Monad m
  => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fac fbc (EitherT meab) = meab >>= either fac fbc

--- ReaderT
newtype ReaderT r m a = ReaderT
  { runReaderT :: r -> m a
  }

instance Functor m =>
         Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m =>
         Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT frab) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) frab rma

instance Monad m =>
         Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $
    \r -> do
      a <- rma r
      runReaderT (f a) r

--- Exercises: StateT
newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

-- 1.
instance Functor m =>
         Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s -> fmap ffirst (smas s)
    where
      ffirst (a, s') = (f a, s')

-- 2.
instance Monad m =>
         Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT smfs) <*> sta =
    StateT $
    \s -> do
      (f, s') <- smfs s
      runStateT (fmap f sta) s'

-- 3.
instance Monad m =>
         Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f =
    StateT $
    \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

--- Exercise: Wrap It Up
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ return . const (Right (Just 1))

-- instance MonadTrans (ReaderT r) where
--   lift = ReaderT . const
-- lift    :: Monad m => m a -> t m a
-- v       :: Monad m => m a
-- const   :: a -> r -> a
-- const $ v :: r -> m a
-- ReaderT . const $ v :: ReaderT r m a
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

--- Exercises: Lift More
-- 1.
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

-- 2.
instance MonadTrans (StateT s) where
  lift ma =
    StateT $
    \s -> do
      a <- ma
      return (a, s)

--- Exercises: Some Instances
-- 1.
instance (MonadIO m) =>
         MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2.
instance (MonadIO m) =>
         MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- 3.
instance (MonadIO m) =>
         MonadIO (StateT s m) where
  liftIO = lift . liftIO


--- Hypothetical Exercise
-- compare ReaderT r Maybe and MaybeT (Reader r)

-- a configuration that might fail, works both ways
rm :: ReaderT Int Maybe Int
rm =
  ReaderT $
  \r ->
     if r > 0
       then Just (r + 1)
       else Nothing

mr :: MaybeT ((->) Int) Int
mr =
  MaybeT $
  \r ->
     if r > 0
       then Just (r + 1)
       else Nothing

-- a way to sum over
rm' :: ReaderT [Int] Maybe Int
rm' =
  ReaderT $
  \r ->
     if r == []
       then Nothing
       else Just $ sum r

mr' :: MaybeT ((->) [Int]) Int
mr' =
  MaybeT $
  \r ->
     if null r
       then Nothing
       else Just $ sum r
