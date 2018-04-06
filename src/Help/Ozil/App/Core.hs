{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Help.Ozil.App.Core
  ( O (..)
  , runO
  , evalO
  , execO
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Help.Ozil.App.Config (Config (..))
import Help.Ozil.App.Cmd (Options)

-- | Everything in the app runs inside the O monad.
newtype O a = O { unO :: ReaderT Options (StateT Config IO) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Options
           , MonadState Config
           )

runO :: r -> s -> StateT s (ReaderT r m) a -> m (a, s)
runO r s ma = runReaderT (runStateT ma s) r

evalO :: Monad m => r -> s -> StateT s (ReaderT r m) a -> m a
evalO r s ma = runReaderT (evalStateT ma s) r

execO :: Monad m => r -> s -> StateT s (ReaderT r m) a -> m s
execO r s ma = runReaderT (execStateT ma s) r
