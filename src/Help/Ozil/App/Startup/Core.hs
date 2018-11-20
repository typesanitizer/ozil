{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Help.Ozil.App.Startup.Core
  (
  -- Startup
    Startup (..)
  , runStartup
  , evalStartup
  -- Environment
  , Env
  , options
  , showEnv
  , modifyConfig
  ) where

import Commons

import Help.Ozil.App.Config.Types (Config)
import Help.Ozil.App.Cmd (Command, HasOptCommand (..), Options)

import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)

--------------------------------------------------------------------------------
-- * The Startup monad.

-- | Everything in the app runs inside the Startup monad.
newtype Startup a = Startup { unO :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runStartup :: Options -> Config -> Startup a -> IO (a, Config)
runStartup o c ma = do
  env <- Env o <$> newIORef c
  a <- runReaderT (unO ma) env
  c' <- readIORef (_envConfig env)
  pure (a, c')

evalStartup :: Options -> Config -> Startup a -> IO a
evalStartup a b c = fst <$> runStartup a b c

--------------------------------------------------------------------------------
-- * Environment

data Env = Env
  { _envOptions :: Options
  , _envConfig :: IORef Config
  }
makeFields ''Env

instance HasOptCommand Env Command where
  optCommand = options . optCommand

showEnv :: Startup String
showEnv = liftIO . showEnv' =<< ask

showEnv' :: Env -> IO String
showEnv' (Env a b) = (show a ++) . show <$> readIORef b

modifyConfig :: (Config -> Config) -> Startup ()
modifyConfig f = do
  c <- view config
  liftIO $ modifyIORef c f
