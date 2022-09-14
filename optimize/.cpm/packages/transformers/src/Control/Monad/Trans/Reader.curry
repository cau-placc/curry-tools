module Control.Monad.Trans.Reader where

import Data.Functor.Identity     ( Identity (..) )
import Control.Monad.IO.Class    ( MonadIO (..) )
import Control.Monad.Trans.Class ( MonadTrans (..) )

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f m = ReaderT $ \r -> fmap f (runReaderT m r)

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  mf <*> m = ReaderT $ \r -> (runReaderT mf r) <*> (runReaderT m r)

instance Alternative m => Alternative (ReaderT r m) where
  empty = ReaderT $ const empty
  x <|> y = ReaderT $ \r -> runReaderT x r <|> runReaderT y r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  m >>= f = ReaderT $ \r -> do x <- runReaderT m r
                               runReaderT (f x) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadFail m => MonadFail (ReaderT r m) where
  fail msg = lift (fail msg)

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- | Retrieves the environment.
ask :: Monad m => ReaderT r m r
ask = ReaderT pure

-- | Retrieves the environment with a selector function applied to it.
asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = f <$> ask

-- | Performs a computation in the reader monad (equivalent to asks).
reader :: Monad m => (r -> a) -> ReaderT r m a
reader = asks

-- | Runs a computation in a modified environment.
withReaderT :: (s -> r) -> ReaderT r m a -> ReaderT s m a
withReaderT f m = ReaderT $ \s -> runReaderT m (f s)

type Reader r = ReaderT r Identity

-- | Runs a computation in the reader monad.
runReader :: Reader r a -> r -> a
runReader m = runIdentity . runReaderT m
