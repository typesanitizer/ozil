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
import qualified Brick.Widgets.Border as Border
import qualified Control.Lens as L
import qualified Graphics.Vty
import qualified Graphics.Vty.Input as V

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
  deriving (Eq, Ord, Show)

type OApp = Brick.App OState OEvent OResource

oapp :: OApp
oapp = Brick.App
  { appDraw = const [ui]
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ Brick.attrMap Graphics.Vty.defAttr []
  }

handleEvent :: s -> Brick.BrickEvent n e -> Brick.EventM OResource (Brick.Next s)
handleEvent s = \case
  Brick.VtyEvent (V.EvKey V.KEsc []) -> Brick.halt s
  Brick.VtyEvent (V.EvKey V.KDown []) -> do
    Brick.vScrollBy (Brick.viewportScroll (OResource 10)) 1
    Brick.continue s
  Brick.VtyEvent (V.EvKey V.KUp []) -> do
    Brick.vScrollBy (Brick.viewportScroll (OResource 10)) (-1)
    Brick.continue s
  _ -> Brick.continue s

-- The UI should look like
--
-- +------ binaryname ------+
-- |                        |
-- | blah                   |
-- |                        |
-- | blah                   |
-- |                        |
-- | blah                   |
-- |                        |
-- +------------------------+
-- | ESC = Exit             |
-- +------------------------+
--
-- ui :: (Show n, Ord n) => Brick.Widget n
ui :: Brick.Widget OResource
ui = Border.borderWithLabel (Brick.str " binaryname ") $
  body
  Brick.<=>
  Border.hBorder
  Brick.<=>
  Brick.str "ESC = Exit"
  -- Brick.<=>
  -- Brick.padTop (Brick.Pad 1) t2
  where
    -- body = Brick.strWrap dummyText
    body = Brick.visible $ Brick.viewport (OResource 10) Brick.Vertical (Brick.strWrap dummyText)
    -- settings = defaultWrapSettings { preserveIndentation = True }
    -- t2 = Brick.strWrapWith settings $
    --     "This text wraps\n" <>
    --     "   with different settings to preserve indentation\n" <>
    --     "   so that long lines wrap in nicer way."

dummyText :: String
dummyText =
  "Case read they must it of cold that. Speaking trifling an to unpacked moderate debating learning. An particular contrasted he excellence favourable on. Nay preference dispatched difficulty continuing joy one. Songs it be if ought hoped of. Too carriage attended him entrance desirous the saw. Twenty sister hearts garden limits put gay has. We hill lady will both sang room by. Desirous men exercise overcame procured speaking her followed. \

\In to am attended desirous raptures declared diverted confined at. Collected instantly remaining up certainly to necessary as. Over walk dull into son boy door went new. At or happiness commanded daughters as. Is handsome an declared at received in extended vicinity subjects. Into miss on he over been late pain an. Only week bore boy what fat case left use. Match round scale now sex style far times. Your me past an much. \

\Am increasing at contrasted in favourable he considered astonished. As if made held in an shot. By it enough to valley desire do. Mrs chief great maids these which are ham match she. Abode to tried do thing maids. Doubtful disposed returned rejoiced to dashwood is so up. \

\Knowledge nay estimable questions repulsive daughters boy. Solicitude gay way unaffected expression for. His mistress ladyship required off horrible disposed rejoiced. Unpleasing pianoforte unreserved as oh he unpleasant no inquietude insipidity. Advantages can discretion possession add favourable cultivated admiration far. Why rather assure how esteem end hunted nearer and before. By an truth after heard going early given he. Charmed to it excited females whether at examine. Him abilities suffering may are yet dependent. \

\Next his only boy meet the fat rose when. Do repair at we misery wanted remove remain income. Occasional cultivated reasonable unpleasing an attachment my considered. Having ask and coming object seemed put did admire figure. Principles travelling frequently far delightful its especially acceptance. Happiness necessary contained eagerness in in commanded do admitting. Favourable continuing difficulty had her solicitude far. Nor doubt off widow all death aware offer. We will up able in both do sing.\

\An country demesne message it. Bachelor domestic extended doubtful as concerns at. Morning prudent removal an letters by. On could my in order never it. Or excited certain sixteen it to parties colonel. Depending conveying direction has led immediate. Law gate her well bed life feet seen rent. On nature or no except it sussex.\

\Built purse maids cease her ham new seven among and. Pulled coming wooded tended it answer remain me be. So landlord by we unlocked sensible it. Fat cannot use denied excuse son law. Wisdom happen suffer common the appear ham beauty her had. Or belonging zealously existence as by resources.\

\Am finished rejoiced drawings so he elegance. Set lose dear upon had two its what seen. Held she sir how know what such whom. Esteem put uneasy set piqued son depend her others. Two dear held mrs feet view her old fine. Bore can led than how has rank. Discovery any extensive has commanded direction. Short at front which blind as. Ye as procuring unwilling principle by.\

\Game of as rest time eyes with of this it. Add was music merry any truth since going. Happiness she ham but instantly put departure propriety. She amiable all without say spirits shy clothes morning. Frankness in extensive to belonging improving so certainty. Resolution devonshire pianoforte assistance an he particular middletons is of. Explain ten man uncivil engaged conduct. Am likewise betrayed as declared absolute do. Taste oh spoke about no solid of hills up shade. Occasion so bachelor humoured striking by attended doubtful be it.\

\For norland produce age wishing. To figure on it spring season up. Her provision acuteness had excellent two why intention. As called mr needed praise at. Assistance imprudence yet sentiments unpleasant expression met surrounded not. Be at talked ye though secure nearer."
