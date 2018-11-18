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

import Help.Page
  (getNewSubcommand, highlightedSubcommand, LinkState, mkLinkStateOff, DocPage)
import Help.Ozil.App.Config.Watch (WatchManager, FSEvent)
import Help.Ozil.App.Config.Types (Config)
import Help.Ozil.App.Cmd (optCommand, Options, HasDebugMode(..), _Default)

import Brick (App (..))
import Brick.BChan (BChan)
import Data.Focused (Focused)
import Lens.Micro.Type (SimpleGetter)

import qualified Data.Focused as F

--------------------------------------------------------------------------------
-- * Data types

type OApp = Brick.App OState OEvent OResource

newtype OEvent = OEvent FSEvent

data OResource
  = TextViewport
  | KeyBindingsViewport
  deriving (Eq, Ord, Show)

data OWatch
  = Uninitialized !WatchManager
  | Running       !(IO ()) -- ^ Action to stop the watch
  | NoWatch

data View = View { _viewDocPage :: !DocPage, _viewLinkState :: !LinkState }

data OState = OState
  { oStateOptions    :: !Options
  , _oStateConfig    :: !Config
  , _oStateViews     :: !(Focused View)
  , _oStateWatch     :: !OWatch
  , oStateChan       :: !(BChan OEvent)
  , _oStateHeading   :: !Text
  , _oStateDebugMode :: !Bool
  }

makeFields ''View
makeFields ''OState

class HasDoc s d | s -> d where
  doc :: SimpleGetter s d

instance HasDoc OState DocPage where
  doc = views . F.focusL . docPage

instance HasLinkState OState LinkState where
  linkState = views . F.focusL . linkState

--------------------------------------------------------------------------------
-- * Operations

mkView :: DocPage -> View
mkView d = View d (mkLinkStateOff d)

-- | Use the existing LinkState to push the highlighted document onto the stack.
pushDoc :: OState -> IO OState
pushDoc s = case highlightedSubcommand (s ^. linkState) d of
  Nothing -> pure s
  Just subc -> do
    d' <- getNewSubcommand subc d
    let mod_f dp = F.clipPushBy ((==) `on` (^. docPage)) (mkView dp)
    pure (maybe s (\dp -> over views (mod_f dp) s) d')

  where d = s ^. views . F.focusL . docPage

popDoc :: OState -> OState
popDoc = over views F.tryPop

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
  , _oStateViews     = F.singleton (mkView dp)
  , _oStateWatch     = Uninitialized wm
  , oStateChan       = ch
  , _oStateHeading   = "binaryname"
  , _oStateDebugMode = fromMaybe False (opts ^? optCommand._Default.debugMode)
  }
