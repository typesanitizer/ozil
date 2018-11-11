{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Help.Ozil.App.Startup
  ( finishStartup
  ) where

import Commons

import Help.Page
import Help.Ozil.App.Cmd
import Help.Ozil.App.Death
import Help.Ozil.App.Startup.Core

import Help.Page.Man (parseWhatisDescription, WhatisDescription (..))
import Help.Ozil.App.Config (getConfig, Config)

import qualified Help.Ozil.App.Default as Default

import Brick (App (..))
import Codec.Compression.GZip (decompress)
import Data.List.Extra (trim)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, dropExtension)
import System.Process (readProcess, readProcessWithExitCode)

import qualified Brick
import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
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
    [dp] -> (, cfg) <$> getDocPage dp
    h:tl -> do
      (dp, ss) <- runSelectionApp (h:|tl)
      (, save ss dp cfg) <$> getDocPage dp
  where
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
  cmd <- L.view optCommand
  case cmd ^? _Default.inputs of
    Nothing -> pure mempty
    Just InputPath{} -> unimplementedErrorM
    Just p@InputFile{} ->
      liftIO $ do
      -- FIXME: whatis may not recognize everything (if mandb hasn't been run
      -- recently), so we might actually need to run man as well
      (ecode, out, _) <- readProcessWithExitCode "whatis" ["-w", go p] ""
      pure $ case ecode of
        ExitFailure _ -> []
        ExitSuccess   -> map ManPageSummary (lines out)
        -- ^ No need to parse it right away.
  where
    go InputPath{} = unimplementedError
    go (InputFile ty name) = case ty of
      Binary           -> name
      ManPage Unzipped -> dropExtension name
      ManPage Zipped   -> dropExtension (dropExtension name)

-- TODO: Extend this to allow for multiple help pages.
-- For example, if you're working on something which you also install
-- globally, then running
-- @stack exec foo -- --help@ VS @foo --help@
-- may give different results.
getHelpPageSummaries :: HasCallStack => Startup [HelpPageSummary]
getHelpPageSummaries = do
  cmd <- L.view optCommand
  check (cmd ^? _Default.inputs) $ \case
    InputPath{} -> unimplementedErrorM
    InputFile ManPage{} _ -> pure []
    InputFile Binary name -> liftIO $ do
      -- FIXME: Calling 'which' is not portable.
      -- https://unix.stackexchange.com/q/85249/89474
      -- However, 'command' might be a shell built-in, and I'm not sure how to
      -- use the API in System.Process to call shell commands and capture stdout
      binpaths <- readProcessSimple "which" [name]
      check binpaths $ \txt ->
        fmap catMaybes . forM (T.lines txt) $ \path -> do
          let path' = unpack path
              tryGettingHelp f b = do
                h <- f path'
                pure (HelpPageSummary path' b <$> (headMaybe =<< fmap T.lines h))
          tryGettingHelp getShortHelp True >>= \case
            Nothing -> tryGettingHelp getLongHelp False
            Just h  -> pure (Just h)
  where
    check x f = case x of Nothing -> pure []; Just z -> f z

--------------------------------------------------------------------------------
-- * Fetching documentation

getDocPage :: DocPageSummary -> IO DocPage
getDocPage = \case
  ManSummary  m -> getManPage m
  HelpSummary h -> getHelpPage h

getManPage :: HasCallStack => ManPageSummary -> IO DocPage
getManPage (ManPageSummary descr) = do
  -- TODO: Error handling...
  let WhatisDescription n s _ = fromJust' (parseWhatisDescription $ T.pack descr)
  path <- trim <$> readProcess "man" ["-S", T.unpack s, "-w", T.unpack n] ""
  -- TODO: Man pages might be in some other encoding like Latin1?
  -- TODO: Man pages might be stored in other formats?
  txt <- if takeExtension path == ".gz"
    then T.decodeUtf8 . BS.toStrict . decompress <$> BS.readFile path
    else T.readFile path
  pure (Man (parseMan txt))


getShortHelp :: FilePath -> IO (Maybe Text)
getShortHelp p = readProcessSimple p ["-h"]

getLongHelp :: FilePath -> IO (Maybe Text)
getLongHelp p = readProcessSimple p ["--help"]

getHelpPage :: HasCallStack => HelpPageSummary -> IO DocPage
getHelpPage (HelpPageSummary binpath short _) =
  let (parse, get) = if short
        then (ShortHelp . parseShortHelp, getShortHelp)
        else (LongHelp . parseLongHelp, getLongHelp)
  in parse . fromJust' <$> get binpath

--------------------------------------------------------------------------------
-- * Selection process

-- Precondition: The input list has 2+ elements.
runSelectionApp :: NonEmpty DocPageSummary -> IO (DocPageSummary, SaveSelection)
runSelectionApp dps = do
  SelectionState (dp:|_) ss d <-
    Brick.defaultMain selectionApp (SelectionState dps DontSaveSelection Mid)
  assert (d == Done) (pure (dp, ss))
  where
    selectionApp :: App SelectionState () Int
    selectionApp = App
      { appDraw = selectionAppDraw
      , appChooseCursor = Brick.showFirstCursor
      , appHandleEvent = selectionAppHandleEvent
      , appStartEvent = pure
      , appAttrMap = const $ Brick.attrMap Vty.defAttr []
      }

data Status = Mid | Done
  deriving Eq

data SelectionState = SelectionState
  { blurbs     :: NonEmpty DocPageSummary
  , saveChoice :: !SaveSelection
  , status     :: Status
  }

selectionAppDraw :: HasCallStack => SelectionState -> [Brick.Widget Int]
selectionAppDraw = \case
  SelectionState b _ Mid  -> drawSelectionUI b
  SelectionState (b :| tl) _ Done -> assert (null tl) (drawSaveDefaultUI b)
  where
    drawSelectionUI = undefined
    drawSaveDefaultUI = undefined

selectionAppHandleEvent
  :: HasCallStack
  => SelectionState
  -> Brick.BrickEvent Int e
  -> Brick.EventM Int (Brick.Next SelectionState)
selectionAppHandleEvent s e = case s of
  SelectionState b _ Mid -> handleSelection b e
  SelectionState (b :| tl) _ Done -> assert (null tl) (handleSaving b e)
  where
    handleSelection = undefined
    handleSaving = undefined

newtype SaveSelection = MkSaveSelection Bool

pattern SaveSelection, DontSaveSelection :: SaveSelection
pattern SaveSelection = MkSaveSelection True
pattern DontSaveSelection = MkSaveSelection False

-- -- |
-- --
-- -- TODO: Create a simple Brick app to let the user pick what they want.
-- -- Further, ask them if they'd like you to save that default for future
-- -- operations. autoSelection is only used as a stop-gap solution.
-- userSelection
--   :: [ManPageSummary] -> [HelpPageSummary] -> Startup (DocPage, SaveSelection)
-- userSelection = autoSelection

-- autoSelection
--   :: [ManPageSummary] -> [HelpPageSummary] -> Startup (DocPage, SaveSelection)
-- autoSelection ms hs = liftIO $ (, DontSaveSelection) <$>
--   case ms of
--     m:_ -> getManPage m
--     []  -> case hs of
--       h:_ -> getHelpPage h
--       []  -> error "Error: Couldn't find a man page or help page."
--       -- TODO: Improve this error. For starters, we should pluck out the name of
--       -- the input from the monad.
