{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Help.Ozil.App.Core
  (
  -- Core app
    OApp
  , OWatch (..)
  , OEvent (..)
  , OResource (..)
  , OState
  , HasText (..)
  , config
  , watch
  , getOptions
  , getBChan
  , newOState
  ) where

import Commons

import Help.Ozil.App.Config.Watch
import Help.Ozil.App.Config.Types (Config)
import Help.Ozil.App.Cmd (Options)

import qualified Help.Ozil.App.Default as Default

import Brick (App (..))
import Brick.BChan (BChan)

--------------------------------------------------------------------------------
-- * GUI

type OApp = Brick.App OState OEvent OResource

newtype OEvent = OEvent FSEvent

data OResource
  = TextViewport
  | KeyBindingsViewport
  deriving (Eq, Ord, Show)

data OWatch
  = Uninitialized !WatchManager
  | Running       !(IO ()) -- ^ Action to stop the watch

data OState = OState
  { oStateOptions :: !Options
  , _oStateConfig :: !Config
  , _oStateText   :: !Text
  , _oStateWatch  :: !OWatch
  , oStateChan    :: !(BChan OEvent)
  }
makeFields ''OState

getOptions :: OState -> Options
getOptions = oStateOptions

getBChan :: OState -> BChan OEvent
getBChan = oStateChan

newOState :: Options -> WatchManager -> BChan OEvent -> OState
newOState opts wm ch = OState
  { oStateOptions = opts
  , _oStateConfig = Default.config
  , _oStateText   = "Placeholder text"
  , _oStateWatch  = Uninitialized wm
  , oStateChan    = ch
  }
