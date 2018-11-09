{-# LANGUAGE MultiWayIf #-}

module Help.Ozil.App where

import Help.Ozil.App.Cmd
import Help.Ozil.App.Death

import Help.Page (DocPage, ManPageInfo (..), HelpPageInfo (..))
import Help.Ozil.App.Core (oapp, evalO, O, OApp)
import Help.Ozil.App.Config (saveConfig, getConfig, toReactOrNotToReact)

import qualified Help.Ozil.App.Default as Default

import Brick.BChan (BChan)
import Control.Lens ((^?))
import Control.Monad (void, join)
import Control.Monad.IO.Class
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension)
import System.Process (readProcessWithExitCode)

import qualified Brick
import qualified Brick.BChan as BChan
import qualified Control.Lens as L
import qualified Graphics.Vty as Vty
import qualified System.FSNotify as FSNotify

main :: IO ()
main = defaultMain $ \opts -> FSNotify.withManager $ \wm -> do
  chan <- BChan.newBChan maxChanSize
  -- stopWatch <- FSNotify.watchDir wm Default.configDir toReactOrNotToReact writeToChan
  saveState $ Brick.customMain gui (Just chan) oapp (initState opts)
  -- stopWatch
  where
    gui = Vty.mkVty Vty.defaultConfig
    initState = undefined
    maxChanSize = 10
    writeToChan :: FSNotify.Event -> IO ()
    writeToChan = undefined
    saveState = void

runOzil :: Options -> IO (FSNotify.WatchManager -> IO ())
runOzil opts = pure $ \wm ->
  join . FSNotify.watchDir wm Default.configDir toReactOrNotToReact $ \event ->
    evalO opts Default.config
      $   getConfig
      >>  selectPages
      >>= viewPages event
      >>  saveConfig

selectPages :: O a
selectPages = userSelection <$> getManPages <*> getHelpPages

getManPages :: O [ManPageInfo]
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
getHelpPages :: O [HelpPageInfo]
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

viewPages :: FSNotify.Event -> [DocPage] -> O ()
viewPages = unimplementedError
-- viewPage :: Options -> DocPage -> IO ()
-- viewPage = undefined
