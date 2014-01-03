{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module MonadError where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..))
import Control.Applicative

class (Monad m) => MonadError m where
  type Error m :: *
  throwError :: Error m -> m a
  catchError :: m a -> (Error m -> m a) -> m a

instance MonadError (Either e) where
  type Error (Either e) = e
  throwError = Left
  catchError (Right x) _ = Right x
  catchError (Left e) f = f e

instance (MonadError m) => MonadError (MaybeT m) where
  type Error (MaybeT m) = Error m
  throwError = lift . throwError
  catchError m f = MaybeT $ catchError (runMaybeT m) (runMaybeT . f)

instance (MonadError m) => MonadError (StateT s m) where
  type Error (StateT s m) = Error m
  throwError = lift . throwError
  catchError m f = StateT g
    where g s = catchError (runStateT m s) (\e -> runStateT (f e) s)
