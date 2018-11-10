module Help.Ozil.App where

import Help.Ozil.App.Cmd
import Help.Ozil.App.Death

import Help.Page (DocPage, ManPageInfo (..), HelpPageInfo (..))
import Help.Ozil.App.Core
  ( HasText (..), getBChan, watch, OResource (..), OState, OEvent (..)
  , newOState, OApp, OWatch (..)
  )
import Help.Ozil.App.Config (FSEvent, saveConfig, getConfig, toReactOrNotToReact)
import Help.Ozil.App.Startup (Startup, evalO)

import qualified Help.Ozil.App.Default as Default

import Brick (App (..))
import Control.Lens ((^?), (^.))
import Control.Monad (when, void, join)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Text (Text)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension)
import System.Process (readProcessWithExitCode)

import qualified Brick
import qualified Brick.BChan as BChan
import qualified Brick.Widgets.Border as Border
import qualified Control.Lens as L
import qualified Graphics.Vty as Vty
import qualified System.FSNotify as FSNotify

main :: IO ()
main = defaultMain $ \opts -> FSNotify.withManager $ \wm -> do
  chan <- BChan.newBChan maxChanSize
  saveState $ Brick.customMain gui (Just chan) oapp (newOState opts wm chan)
  where
    gui = Vty.mkVty Vty.defaultConfig
    maxChanSize = 20
    saveState = void

oapp :: OApp
oapp = Brick.App
  { appDraw = \s -> [ui s]
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = ozilStartEvent
  , appAttrMap = const $ Brick.attrMap Vty.defAttr []
  }

runOzil :: Options -> IO (FSNotify.WatchManager -> IO ())
runOzil opts = pure $ \wm ->
  join . FSNotify.watchDir wm Default.configDir toReactOrNotToReact $ \event ->
    evalO opts Default.config
      $   getConfig
      >>  selectPages
      >>= viewPages event
      >>  saveConfig

selectPages :: Startup a
selectPages = userSelection <$> getManPages <*> getHelpPages

getManPages :: Startup [ManPageInfo]
getManPages = do
  cmd <- L.view optCommand
  case cmd ^? _Default.inputs of
    Nothing -> pure mempty
    Just (_ :| _ : _) -> unreachableError
    Just (InputPath{} :| []) -> unimplementedErrorM
    Just (p@InputFile{} :| []) ->
      liftIO $ do
      (ecode, out, _) <- readProcessWithExitCode "whatis" ["-w", go p] ""
      case ecode of
        ExitFailure _ -> pure []
        ExitSuccess -> pure $ map ManPageInfo (lines out)
        -- ^ No need to parse it right away.
  where
    go InputPath{} = unimplementedError
    go (InputFile ty name) = case ty of
      Binary -> name
      ManPage Unzipped -> dropExtension name
      ManPage Zipped -> dropExtension (dropExtension name)

-- TODO: Extend this to allow for multiple help pages.
-- For example, if you're working on something which you also install
-- globally, then running
-- @stack exec foo -- --help@ VS @foo --help@
-- may give different results.
getHelpPages :: Startup [HelpPageInfo]
getHelpPages = do
  cmd <- L.view optCommand
  case cmd ^? _Default.inputs of
    Nothing -> pure mempty
    Just (_ :| _ : _) -> unreachableErrorM
    Just (p :| []) -> case p of
      InputFile ManPage{} _ -> pure []
      InputFile Binary _ -> unimplementedErrorM
      -- liftIO $ do
      -- (ecode, out, _) <- readProcessWithExitCode "which" [name] ""
      -- case ecode of
      --   ExitFailure _ -> pure []
      --   ExitSuccess -> pure $ map HelpPageInfo
      InputPath{} -> unimplementedErrorM

-- userSelection :: -> [DocPage]
userSelection :: a
userSelection = unimplementedError

viewPages :: FSNotify.Event -> [DocPage] -> Startup ()
viewPages = unimplementedError
-- viewPage :: Options -> DocPage -> IO ()
-- viewPage = undefined

ozilStartEvent :: OState -> Brick.EventM OResource OState
ozilStartEvent s = case s ^. watch of
  Running _ -> pure s
  Uninitialized wm -> do
    -- TODO: Write path creating logic
    -- mkdir with parents
    -- Tell user that you made a directory :)
    -- Pause for a bit so they can read the message :)
    -- Go ahead.
    sw <- liftIO
      (FSNotify.watchDir wm Default.configDir toReactOrNotToReact forwardEvent)
    pure (L.set watch (Running sw) s)
  where
    forwardEvent :: FSEvent -> IO ()
    forwardEvent = BChan.writeBChan (getBChan s) . OEvent

handleEvent
  :: OState
  -> Brick.BrickEvent n OEvent
  -> Brick.EventM OResource (Brick.Next OState)
handleEvent s = \case
  Brick.VtyEvent (Vty.EvKey Vty.KEsc []) -> do
    case s ^. watch of
      Running stopWatch -> liftIO stopWatch
      Uninitialized _ -> pure ()
    Brick.halt s
  ev -> do
    let
      scrollAmt = case ev of
        Brick.VtyEvent (Vty.EvKey Vty.KDown []) ->  1
        Brick.VtyEvent (Vty.EvKey Vty.KUp   []) -> -1
        _ -> 0
    when (scrollAmt /= 0)
      $ Brick.vScrollBy (Brick.viewportScroll TextViewport) scrollAmt
    Brick.continue s

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
ui :: HasText s Text => s -> Brick.Widget OResource
ui s = Border.borderWithLabel (Brick.str " binaryname ") $
  body
  Brick.<=>
  Border.hBorder
  Brick.<=>
  Brick.txt "ESC = Exit"
  where
    body = s ^. text
      & Brick.txtWrap
      & Brick.viewport TextViewport Brick.Vertical
