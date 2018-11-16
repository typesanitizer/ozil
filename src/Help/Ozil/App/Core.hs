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
  , pushDoc
  , popDoc
  , HasHeading (..)
  , HasLinkState (..)
  , HasDebugMode (..)
  , config
  , watch
  , getOptions
  , getBChan
  , newOState
  ) where

import Commons hiding (to)

import Help.Page (LinkState, mkLinkStateOff, DocPage)
import Help.Ozil.App.Config.Watch (WatchManager, FSEvent)
import Help.Ozil.App.Config.Types (Config)
import Help.Ozil.App.Cmd (optCommand, Options, HasDebugMode(..), _Default)

import Brick (App (..))
import Brick.BChan (BChan)
import Data.Focused (Focused)
import Lens.Micro (to)
import Lens.Micro.Type (SimpleGetter)

import qualified Data.Focused as F

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
  , _oStateDocs      :: !(Focused DocPage)
  , _oStateWatch     :: !OWatch
  , oStateChan       :: !(BChan OEvent)
  , _oStateHeading   :: !Text
  , _oStateLinkState :: !LinkState
  , _oStateDebugMode :: !Bool
  }
makeFields ''OState

class HasDoc s d | s -> d where
  doc :: SimpleGetter s d

pushDoc :: DocPage -> OState -> OState
pushDoc = over docs . F.clipPushBy (\_ _ -> False)

popDoc :: OState -> OState
popDoc = over docs F.tryPop

instance HasDoc OState DocPage where
  doc = to (F.focus . _oStateDocs)

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
  , _oStateDocs      = F.singleton dp
  , _oStateWatch     = Uninitialized wm
  , oStateChan       = ch
  , _oStateHeading   = "binaryname"
  , _oStateLinkState = mkLinkStateOff dp
  , _oStateDebugMode = fromMaybe False (opts ^? optCommand._Default.debugMode)
  }
