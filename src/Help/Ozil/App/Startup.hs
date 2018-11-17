{-# LANGUAGE ViewPatterns #-}

module Help.Ozil.App.Startup
  ( finishStartup
  ) where

import Commons

import Help.Page
import Help.Ozil.App.Cmd
import Help.Ozil.App.Death
import Help.Ozil.App.Startup.Core

import Help.Page.Lenses (name, section, shortDescription)
import Help.Ozil.App.Config (getConfig, Config)

import qualified Help.Ozil.App.Default as Default

import System.FilePath

import Brick (App (..))
import Lens.Micro ((^?!))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import qualified Brick
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Dialog as W
import qualified Brick.Widgets.GDialog as W
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

--------------------------------------------------------------------------------
-- * Exports

-- | Run the startup "application", returning the docpage to be views and the
-- proper configuration.
--
-- NOTE: This function might kill the application because we actually don't want
-- to view any documents at all.
finishStartup :: Options -> IO (DocPage, Config)
finishStartup o = do
  (dps, cfg) <- runStartup o Default.config (getConfig >> getCandidates)
  case dps of
    []   -> error "Empty list"
    [dp] -> (, cfg) . fromMaybe err <$> getDocPage dp
    h:tl -> do
      (dp, ss) <- runSelectionApp (h:|tl)
      (, save ss dp cfg) . fromMaybe err <$> getDocPage dp
  where
    err = error "Error: Expected to find a documentation page but couldn't.\n\
                \This seems impossible, so what went wrong :(."
    save ss = if coerce ss then savePreferredCandidate else flip const

--------------------------------------------------------------------------------
-- * Fetching summaries

getCandidates :: HasCallStack => Startup [DocPageSummary]
getCandidates = getPreferredCandidate >>= \case
  Just x  -> pure [x]
  Nothing -> do
    ms <- getManPageSummaries
    hs <- getHelpPageSummaries
    pure (map ManSummary ms ++ map HelpSummary hs)

-- TODO: Check the Config if it already has a default for the request binary.
-- If we have a saved default, and it is available in the filesystem, then
-- return it. Otherwise, go through the effort of checking stuff.
getPreferredCandidate :: Startup (Maybe DocPageSummary)
getPreferredCandidate = pure Nothing

-- TODO: Modify the config appropriately...
savePreferredCandidate :: DocPageSummary -> Config -> Config
savePreferredCandidate _ = id

getManPageSummaries :: HasCallStack => Startup [ManPageSummary]
getManPageSummaries = do
  cmd <- view optCommand
  case cmd ^? _Default.inputs.primary of
    Nothing -> pure mempty
    Just InputPath{}   -> unimplementedErrorM
    Just p@InputFile{} ->
      liftIO $ do
      -- FIXME: whatis may not recognize everything (if mandb hasn't been run
      -- recently), so we might actually need to run man as well.
      (ecode, out, _) <- readProcessWithExitCode "whatis" ["-w", go p] ""
      pure $ case ecode of
        ExitFailure _ -> []
        ExitSuccess   -> map parseManPageSummary (lines out)
  where
    go InputPath{} = unimplementedError
    go (InputFile ty nm) = case ty of
      Binary           -> nm
      ManPage Unzipped -> dropExtension nm
      ManPage Zipped   -> dropExtension (dropExtension nm)

-- TODO: Extend this to allow for multiple help pages.
-- For example, if you're working on something which you also install
-- globally, then running
-- @stack exec foo -- --help@ VS @foo --help@
-- may give different results.
getHelpPageSummaries :: HasCallStack => Startup [HelpPageSummary]
getHelpPageSummaries = do
  cmd <- view optCommand
  check (cmd ^? _Default.inputs.primary) $ \case
    InputPath{} -> unimplementedErrorM
    InputFile i@ManPage{} _ -> liftIO (print i) >> pure []
    InputFile Binary nm -> liftIO $ do
      -- FIXME: Calling 'which' is not portable.
      -- https://unix.stackexchange.com/q/85249/89474
      -- However, 'command' might be a shell built-in, and I'm not sure how to
      -- use the API in System.Process to call shell commands and capture stdout.
      sys_binpaths <- readProcessSimple "which" [nm]
      check sys_binpaths $ \txt ->
        fmap catMaybes . forM (T.lines txt) $ \path -> do
          let rest = cmd ^?! _Default.inputs.subcommandPath
              path' = unpack path -- <> rest
          getHelpPageSummary (Simple path') rest
  where
    check x f = case x of Nothing -> pure []; Just z -> f z

--------------------------------------------------------------------------------
-- * Selection process

runSelectionApp
  :: NonEmpty DocPageSummary
  -> IO (DocPageSummary, SaveSelection)
runSelectionApp dps = do
  let len = NE.length dps
  i <- Brick.defaultMain (selectionApp len dps) 0
  let dp = assert (0 <= i && i < len) (dps NE.!! i)
  ss <- Brick.defaultMain (saveSelectionApp dp) DontSave
  pure (dp, ss)

highlightSelection :: Brick.AttrMap
highlightSelection = Brick.attrMap Vty.defAttr
  [(W.buttonSelectedAttr, Vty.withStyle Vty.defAttr Vty.standout)]

------------------------------------------------------------
-- ** Main selection app

selectionApp :: Int -> NonEmpty DocPageSummary -> App Int () Int
selectionApp len ds = App
  { appDraw = selectionAppDraw ds
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = selectionAppHandleEvent len
  , appStartEvent = pure
  , appAttrMap = const highlightSelection
  }

selectionAppDialogWidth :: Int
selectionAppDialogWidth = 60

selectionAppDraw :: NonEmpty DocPageSummary -> Int -> [Brick.Widget n]
selectionAppDraw ds i =
  [ W.renderGDialog
    ( W.VDialog $ W.dialog
      (Just " So many options! Pick one. ")
      (Just (i, buttons))
      selectionAppDialogWidth
    ) W.emptyWidget
  ]
  where buttons = map (\d -> (summaryButtonStr d, d)) (NE.toList ds)

selectionAppHandleEvent
  :: Int -- ^ Length of the list
  -> Int -- ^ State (index into the list)
  -> Brick.BrickEvent n e
  -> Brick.EventM n' (Brick.Next Int)
selectionAppHandleEvent len i = \case
  Brick.VtyEvent ev ->
    case W.simpleHandleEvent p W.V ev of
      W.Next i' -> Brick.continue i'
      W.Done    -> Brick.halt i
      W.Unknown -> Brick.continue i
  _ -> Brick.continue i
  where p = W.Pos{W.idx=i, W.len}

------------------------------------------------------------
-- ** Save dialog box

saveSelectionApp :: DocPageSummary -> App SaveSelection () Int
saveSelectionApp dp = App
  { appDraw = saveSelectionAppDraw dp
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = saveSelectionAppHandleEvent
  , appStartEvent = pure
  , appAttrMap = const highlightSelection
  }

saveSelectionAppDraw :: p -> SaveSelection -> [Brick.Widget n]
saveSelectionAppDraw _ ss =
  [ W.renderGDialog
    ( W.HDialog $ W.dialog
      (Just " Would you like to save this choice for the future? ")
      (Just (fromEnum ss, buttons))
      60
    ) W.emptyWidget
  ]
  -- TODO: Add an option for "No. Don't prompt me again for this in the future."
  -- Maybe that should only be an option in the config file and not in the TUI?
  where buttons = [("Yes", Save), ("No", DontSave)]

saveSelectionAppHandleEvent
  :: SaveSelection
  -> Brick.BrickEvent n1 e
  -> Brick.EventM n2 (Brick.Next SaveSelection)
saveSelectionAppHandleEvent s = \case
  Brick.VtyEvent ev ->
    case W.simpleHandleEvent p W.H ev of
      W.Next i' -> Brick.continue (toEnum i')
      W.Done    -> Brick.halt s
      W.Unknown -> Brick.continue s
  _ -> Brick.continue s
  where p = W.Pos{W.idx=fromEnum s, W.len=2}

summaryButtonStr :: DocPageSummary -> String
summaryButtonStr = \case
  HelpSummary h -> displayHelpPageSummary h
  ManSummary (UnknownFormat s) -> s
  ManSummary (WhatisDescr w) ->
    let s1 = printf "%s(%s)" (w ^. name) (w ^. section) :: String
        (s2pre, splitAt 3 -> (post', post'')) =
          splitAt (selectionAppDialogWidth - length s1 - 8) (w ^. shortDescription)
        s2 = s2pre ++ (if null post'' then post' else "...")
    in printf "%s - %s" s1 s2

newtype SaveSelection = SaveSelection Bool
  deriving Eq

instance Enum SaveSelection where
  toEnum = \case
    0 -> Save
    1 -> DontSave
    _ -> unreachableError

  fromEnum (SaveSelection b) = if b then 0 else 1

{-# COMPLETE Save, DontSave #-}

pattern Save, DontSave :: SaveSelection
pattern Save = SaveSelection True
pattern DontSave = SaveSelection False
