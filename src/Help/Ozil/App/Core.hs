{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Help.Ozil.App.Core
  ( O (..)
  , Env
  , options
  , config
  , showEnv
  , modifyConfig
  , runO
  , evalO
  , execO
  , OApp
  , OEvent (..)
  , OState (..)
  , OResource (..)
  , oapp
  ) where

import Help.Ozil.App.Config.Watch (Event)
import Help.Ozil.App.Config.Types (Config (..))
import Help.Ozil.App.Cmd (Command, HasOptCommand(..), Options)

import Brick (App (..))
import Control.Lens.TH (makeFields)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.IO.Class
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)
import Text.Wrap (defaultWrapSettings, preserveIndentation)

import qualified Brick
import qualified Control.Lens as L

--------------------------------------------------------------------------------
-- * Environment

data Env = Env
  { _envOptions :: Options
  , _envConfig :: IORef Config
  }
makeFields ''Env

instance HasOptCommand Env Command where
  optCommand = options . optCommand

showEnv :: O String
showEnv = liftIO . showEnv' =<< ask

showEnv' :: Env -> IO String
showEnv' (Env a b) = (show a ++) . show <$> readIORef b

modifyConfig :: (Config -> Config) -> O ()
modifyConfig f = do
  c <- L.view config
  liftIO $ modifyIORef c f

--------------------------------------------------------------------------------
-- * The O monad.

-- | Everything in the app runs inside the O monad.
newtype O a = O { unO :: ReaderT Env IO a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
           )

runO :: Options -> Config -> O a -> IO (a, Config)
runO o c ma = do
  env <- Env o <$> newIORef c
  a <- runReaderT (unO ma) env
  c' <- readIORef (_envConfig env)
  pure (a, c')

evalO :: Options -> Config -> O a -> IO a
evalO a b c = fst <$> runO a b c

execO :: Options -> Config -> O a -> IO Config
execO a b c = snd <$> runO a b c

--------------------------------------------------------------------------------
-- * GUI

newtype OEvent = OEvent Event

newtype OState = OState Env

newtype OResource = OResource Int
  deriving (Eq, Ord)

type OApp = Brick.App OState OEvent OResource

oapp :: OApp
oapp = Brick.App
  { appDraw = const [ui]
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = undefined
  , appStartEvent = undefined
  , appAttrMap = undefined
  }

ui :: Brick.Widget n
ui = t1 Brick.<=> Brick.padTop (Brick.Pad 1) t2
  where
    t1 = Brick.strWrap $ "Hello, world! This line is long enough that " <>
                   "it's likely to wrap on your terminal if your window " <>
                   "isn't especially wide. Try narrowing and widening " <>
                   "the window to see what happens to this text."
    settings = defaultWrapSettings { preserveIndentation = True }
    t2 = Brick.strWrapWith settings $
        "This text wraps\n" <>
        "   with different settings to preserve indentation\n" <>
        "   so that long lines wrap in nicer way."
