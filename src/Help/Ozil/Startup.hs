{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Help.Ozil.Startup
  ( finishStartup
  ) where

import Commons

import Help.Subcommand (Subcommand)
import Help.Page
import Help.Ozil.Cmd
import Help.Ozil.Startup.Core

import Help.Page.Lenses (name, section, shortDescription)
import Help.Ozil.Config (getConfig, Config, saveConfig)
import Help.Ozil.Config.Types (getPagePath, mkChoice, savedPreferences, userConfig)

import qualified Help.Ozil.Config.Default as Default

import System.FilePath

import Brick (App (..))
import Data.Either (rights)
import Lens.Micro ((^?!))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import qualified Brick
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Dialog as W
import qualified Brick.Widgets.GDialog as W
import qualified Data.HashMap.Strict as H
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
    []   -> error "There were no candidates for the page that you search for.\n\
                  \Perhaps you made a typo?"
    [dp] -> (, cfg) . fromMaybe err <$> getDocPage dp
    h:tl -> do
      (dp, ss) <- runSelectionApp (h:|tl)
      let cfg' = save ss (h:|tl) cfg
      when (_save ss) $ void (saveConfig o cfg')
      (, cfg') . fromMaybe err <$> getDocPage dp
  where
    err = error "Error: Expected to find a documentation page but couldn't.\n\
                \This seems impossible, so what went wrong :(."
    inputs_ = o ^?! optCommand._Default.inputs
    pref_txt = case inputs_ ^. primary of
      InputPath{} -> unreachableError
      InputFile (ManPage _) _ -> unreachableError
      InputFile Binary fname ->
        mkPreferenceText fname (inputs_ ^. subcommandPath)
    save ss dps =
      if _save ss
      then savePreferredCandidate (_idx ss) dps pref_txt
      else id

--------------------------------------------------------------------------------
-- * Fetching summaries

getCandidates :: HasCallStack => Startup [DocPageSummary]
getCandidates = getPreferredCandidate >>= \case
  Just x  -> pure [x]
  Nothing -> do
    ms <- getManPageSummaries
    hs <- getHelpPageSummaries
    pure (map ManSummary ms ++ map HelpSummary hs)

mkPreferenceText :: FileName -> [Subcommand] -> Text
mkPreferenceText fname subcs =
  T.intercalate " " (pack fname : map (pack . show) subcs)

-- TODO: Check the Config if it already has a default for the request binary.
-- If we have a saved default, and it is available in the filesystem, then
-- return it. Otherwise, go through the effort of checking stuff.
getPreferredCandidate :: Startup (Maybe DocPageSummary)
getPreferredCandidate = do
  opts <- view options
  let inputs_ = opts ^?! optCommand._Default.inputs
  case inputs_ ^. primary of
    InputPath{} -> pure Nothing
    InputFile (ManPage _) _ -> pure Nothing
    InputFile Binary fname -> do
      let txt = mkPreferenceText fname (inputs_ ^. subcommandPath)
          subcs = inputs_ ^. subcommandPath
      choice <- inspectConfig
        (\cfg -> H.lookup txt (cfg ^. userConfig . savedPreferences))
      liftIO (join <$> traverse (getSummary subcs . getPagePath) choice)

savePreferredCandidate
  :: Int -> NonEmpty DocPageSummary -> Text -> Config -> Config
savePreferredCandidate i dps inp_txt =
  over (userConfig . savedPreferences) (H.insert inp_txt (mkChoice i dps))

getManPageSummaries :: HasCallStack => Startup [ManPageSummary]
getManPageSummaries = do
  cmd <- view optCommand
  check (cmd ^? _Default.inputs.primary) $ \case
    InputPath{}   -> unimplementedErrorM
    (InputFile ManPage{} _) -> unimplementedErrorM
    p@InputFile{} ->
      liftIO $ do
      -- FIXME: whatis may not recognize everything (if mandb hasn't been run
      -- recently), so we might actually need to run man as well.
      (ecode, out, _) <- readProcessWithExitCode "whatis" ["-w", go p] ""
      pure $ case ecode of
        ExitFailure _ -> []
        -- TODO: We should log the errors and suggest the user report them
        -- on GitHub, or perhaps we can submit a GitHub request ourselves.
        ExitSuccess   -> rights (map parseManPageSummary (lines out))
  where
    go InputPath{} = unimplementedError
    -- FIXME: This logic is wrong. If someone says man.1 then we should check
    -- section 1 only.
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
    InputFile ManPage{} _ -> pure []
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
          getHelpPageSummary (Global path') rest

check :: (Applicative f, Monoid b) => Maybe a -> (a -> f b) -> f b
check x f = maybe (pure mempty) f x

--------------------------------------------------------------------------------
-- * Selection process

runSelectionApp
  :: NonEmpty DocPageSummary
  -> IO (DocPageSummary, Selection)
runSelectionApp dps = do
  let len = NE.length dps
  i <- Brick.defaultMain (selectionApp len dps) 0
  let dp = assert (0 <= i && i < len) (dps NE.!! i)
  -- TODO: We should use this app and then save the configuration.
  ss <- Brick.defaultMain (saveSelectionApp dp) (Selection i False)
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

saveSelectionApp :: DocPageSummary -> App Selection () Int
saveSelectionApp dp = App
  { appDraw = saveSelectionAppDraw dp
  , appChooseCursor = Brick.showFirstCursor
  , appHandleEvent = saveSelectionAppHandleEvent
  , appStartEvent = pure
  , appAttrMap = const highlightSelection
  }

saveSelectionAppDraw :: p -> Selection -> [Brick.Widget n]
saveSelectionAppDraw _ ss =
  [ W.renderGDialog
    ( W.HDialog $ W.dialog
      (Just " Would you like to save this choice for the future? \n\
            \ (Sorry, this doesn't actually work at the moment.) ")
      (Just (button_idx, buttons))
      60
    ) W.emptyWidget
  ]
  -- TODO: Add an option for "No. Don't prompt me again for this in the future."
  -- Maybe that should only be an option in the config file and not in the TUI?
  where
    button_idx = if _save ss then 0 else 1
    buttons = [("Yes", ss{_save = True}), ("No", ss{_save = False})]

saveSelectionAppHandleEvent
  :: Selection
  -> Brick.BrickEvent n1 e
  -> Brick.EventM n2 (Brick.Next Selection)
saveSelectionAppHandleEvent s = \case
  Brick.VtyEvent ev ->
    case W.simpleHandleEvent p W.H ev of
      W.Next i' -> Brick.continue s{_save = i' == 0}
      W.Done    -> Brick.halt s
      W.Unknown -> Brick.continue s
  _ -> Brick.continue s
  where p = W.Pos { W.idx = if _save s then 0 else 1, W.len = 2 }

summaryButtonStr :: DocPageSummary -> String
summaryButtonStr = \case
  HelpSummary h -> displayHelpPageSummary h
  ManSummary w ->
    let s1 = printf "%s(%s)" (w ^. name) (w ^. section) :: String
        (s2pre, splitAt 3 -> (post', post'')) =
          splitAt (selectionAppDialogWidth - length s1 - 8) (w ^. shortDescription)
        s2 = s2pre ++ (if null post'' then post' else "...")
    in printf "%s - %s" s1 s2

data Selection = Selection { _idx :: !Int, _save :: !Bool }
  deriving (Eq, Show)
