module Help.Ozil.App.Startup
  ( finishStartup
  , SaveSelection
  , pattern SaveSelection
  , pattern DontSaveSelection
  ) where

import Commons

import Help.Page
import Help.Ozil.App.Cmd
import Help.Ozil.App.Death
import Help.Ozil.App.Startup.Core

import Help.Page.Man (parseWhatisDescription, WhatisDescription (..))
import Help.Ozil.App.Config (getConfig, Config)

import qualified Help.Ozil.App.Default as Default

import Codec.Compression.GZip (decompress)
import Data.List.Extra (trim)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, dropExtension)
import System.Process (readProcess, readProcessWithExitCode)

import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS

--------------------------------------------------------------------------------
-- * Exports

-- | Run the startup "application", returning the docpage to be views and the
-- proper configuration.
--
-- NOTE: This function might kill the application because we actually don't want
-- to view any documents at all.
finishStartup :: Options -> IO ((DocPage, SaveSelection), Config)
finishStartup o = runStartup o Default.config (getConfig >> selectPages)

--------------------------------------------------------------------------------

selectPages :: HasCallStack => Startup (DocPage, SaveSelection)
selectPages = do
  liftIO $ print "getting mp"
  mp <- getManPages
  liftIO $ print "getting hp"
  hp <- getHelpPages
  liftIO $ print "run selection"
  userSelection mp hp

getManPages :: HasCallStack => Startup [ManPageInfo]
getManPages = do
  cmd <- L.view optCommand
  case cmd ^? _Default.inputs of
    Nothing -> pure mempty
    Just (_ :| _ : _) -> unreachableError
    Just (InputPath{} :| []) -> unimplementedErrorM
    Just (p@InputFile{} :| []) ->
      liftIO $ do
      -- FIXME: whatis may not recognize everything, so we might actually need
      -- to run man as well
      (ecode, out, _) <- readProcessWithExitCode "whatis" ["-w", go p] ""
      pure $ case ecode of
        ExitFailure _ -> []
        ExitSuccess   -> map ManPageInfo (lines out)
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
getHelpPages :: HasCallStack => Startup [HelpPageInfo]
getHelpPages = do
  cmd <- L.view optCommand
  case cmd ^? _Default.inputs of
    Nothing -> pure mempty
    Just (_ :| _ : _) -> unreachableErrorM
    Just (p :| []) -> case p of
      InputFile ManPage{} _ -> pure []
      InputFile Binary name -> liftIO $ do
        (ecode, out, _) <- readProcessWithExitCode "which" [name] ""
        pure $ case ecode of
          ExitFailure _ -> []
          ExitSuccess   -> map HelpPageInfo (lines out)
      InputPath{} -> unimplementedErrorM

newtype SaveSelection = MkSaveSelection Bool

pattern SaveSelection, DontSaveSelection :: SaveSelection
pattern SaveSelection = MkSaveSelection True
pattern DontSaveSelection = MkSaveSelection False

-- |
--
-- TODO: Create a simple Brick app to let the user pick what they want.
-- Further, ask them if they'd like you to save that default for future
-- operations. autoSelection is only used as a stop-gap solution.
userSelection
  :: [ManPageInfo] -> [HelpPageInfo] -> Startup (DocPage, SaveSelection)
userSelection = autoSelection

autoSelection
  :: [ManPageInfo] -> [HelpPageInfo] -> Startup (DocPage, SaveSelection)
autoSelection ms hs = liftIO $ (, DontSaveSelection) <$>
  case ms of
    m:_ -> retrieveManPage m
    []  -> case hs of
      h:_ -> retrieveHelpPage h
      []  -> error "Error: Couldn't find a man page or help page."
      -- TODO: Improve this error. For starters, we should pluck out the name of
      -- the input from the monad.

retrieveManPage :: ManPageInfo -> IO DocPage
retrieveManPage (ManPageInfo descr) = do
  -- TODO: Error handling...
  let WhatisDescription n s _ = fromJust (parseWhatisDescription $ T.pack descr)
  path <- trim <$> readProcess "man" ["-S", T.unpack s, "-w", T.unpack n] ""
  -- TODO: Man pages might be in some other encoding like Latin1?
  -- TODO: Man pages might be stored in other formats?
  txt <- if takeExtension path == ".gz"
    then T.decodeUtf8 . BS.toStrict . decompress <$> BS.readFile path
    else T.readFile path
  pure (Man (parseMan txt))

retrieveHelpPage :: HelpPageInfo -> IO DocPage
retrieveHelpPage (HelpPageInfo binpath) = do
  (ecode, out, _) <- readProcessWithExitCode binpath ["--help"] ""
  case ecode of
    ExitSuccess   -> pure . LongHelp . parseLongHelp $ T.pack out
    ExitFailure _ -> do
      (ecode', out', _) <- readProcessWithExitCode binpath ["-h"] ""
      case ecode' of
        ExitSuccess -> pure . ShortHelp . parseShortHelp $ T.pack out'
        ExitFailure _ -> error "Error: The thing doesn't have a help page..."
        -- TODO: Improve this error.
