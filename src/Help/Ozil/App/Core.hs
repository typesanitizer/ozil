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
  , HasDoc (..)
  , HasHeading (..)
  , HasLinkState (..)
  , config
  , watch
  , getOptions
  , getBChan
  , newOState
  ) where

import Commons

import Help.Page (LinkState, mkLinkStateOff, DocPage)
import Help.Ozil.App.Config.Watch (WatchManager, FSEvent)
import Help.Ozil.App.Config.Types (Config)
import Help.Ozil.App.Cmd (Options)

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
  { oStateOptions    :: !Options
  , _oStateConfig    :: !Config
  , _oStateDoc       :: !DocPage
  , _oStateWatch     :: !OWatch
  , oStateChan       :: !(BChan OEvent)
  , _oStateHeading   :: !Text
  , _oStateLinkState :: !LinkState
  }
makeFields ''OState

getOptions :: OState -> Options
getOptions = oStateOptions

getBChan :: OState -> BChan OEvent
getBChan = oStateChan

newOState
  :: Options
  -> WatchManager
  -> BChan OEvent
  -> DocPage
  -> Config
  -> OState
newOState opts wm ch dp cfg = OState
  { oStateOptions    = opts
  , _oStateConfig    = cfg
  , _oStateDoc       = dp
  , _oStateWatch     = Uninitialized wm
  , oStateChan       = ch
  , _oStateHeading   = "binaryname"
  , _oStateLinkState = mkLinkStateOff dp
  }
