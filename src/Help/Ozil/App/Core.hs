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
           , MonadIO
           )

runO :: Options -> Config -> O a -> IO (a, Config)
runO r s ma = runStateT (runReaderT (unO ma) r) s

evalO :: Options -> Config -> O a -> IO a
evalO r s ma = evalStateT (runReaderT (unO ma) r) s

execO :: Options -> Config -> O a -> IO Config
execO r s ma = execStateT (runReaderT (unO ma) r) s
