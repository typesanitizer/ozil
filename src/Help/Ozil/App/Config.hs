{-# LANGUAGE RankNTypes      #-}

module Help.Ozil.App.Config
  ( getConfig
  , saveConfig
  , module Help.Ozil.App.Config.Watch
  , Conf.Config
  )
  where

import Commons

import Help.Ozil.App.Cmd (configPath, optCommand, Command (..), ConfigOptions (..))
import Help.Ozil.App.Config.Watch
import Help.Ozil.App.Console.Text (warn, prompt, pattern DefaultYes)
import Help.Ozil.App.Death (unimplementedErrorM, unreachableError, unreachableErrorM, oDie)
import Help.Ozil.App.Startup.Core (options, modifyConfig, Startup)
import System.Directory

import Data.Yaml (prettyPrintParseException, decodeFileEither, encode)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import Text.Printf (printf)

import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Help.Ozil.App.Config.Types as Conf
import qualified Help.Ozil.App.Default as Default

-- TODO: Draw a state diagram of possible transitions and refactor helper
-- functions appropriately. Right now, the linear flow is misleading, and
-- the problem is that sometimes functions need to know more details about
-- implementations of previous functions, which is not reflected in the types.

getConfig :: HasCallStack => Startup ()
getConfig =
  foundConfigFile
    >>= deleteConfigFileIfApplicable
    >>= createConfigFileIfApplicable
    >>= readWriteConfig
    >>  checkDbExists
    >>= syncDbIfApplicable

foundConfigFile :: Startup OzilFileExists
foundConfigFile = do
  p <- view (options . configPath)
  cfe <- liftIO $ case p of
    Nothing -> doesFileExist Default.configPath <&> \case
      True  -> Just Default.configPath
      False -> Nothing
    Just x -> doesFileExist x <&> \case
      True  -> p
      False -> errConfigFileMissing x
  modifyConfig (set Conf.configFileExists cfe)
  pure (view exists (isJust cfe))
  where
    errConfigFileMissing :: FilePath -> a
    errConfigFileMissing x = error
      $ printf "Error: Expected a config file at %s but it wasn't found.\n\
               \Perhaps double-check the path?" x

effectiveConfigPath :: Startup FilePath
effectiveConfigPath =
  fromMaybe Default.configPath <$> view (options . configPath)

data OzilFileExists = OzilFileMissing | OzilFileExists

exists :: L.Iso' Bool OzilFileExists
exists = L.iso
  (\case False -> OzilFileMissing; True -> OzilFileExists)
  (\case OzilFileMissing -> False; OzilFileExists -> True)

deleteConfigFileIfApplicable :: OzilFileExists -> Startup OzilFileExists
deleteConfigFileIfApplicable ozilFileExists = view optCommand >>= \case
  Config ConfigDelete -> delete *> liftIO exitSuccess
  Config ConfigReInit -> delete *> pure OzilFileMissing
  Config ConfigSync   -> pure ozilFileExists
  Config ConfigInit   -> pure ozilFileExists
  WhatIs _            -> pure ozilFileExists
  Default _           -> pure ozilFileExists
 where
  delete = when (ozilFileExists ^. L.from exists) $ do
    liftIO $ removePathForcibly Default.configPath
    modifyConfig (set Conf.configFileExists Nothing)

createConfigFileIfApplicable :: HasCallStack => OzilFileExists -> Startup OzilFileExists
createConfigFileIfApplicable ozilFileExists = view optCommand >>= \case
  Default{}           -> promptInit
  WhatIs{}            -> pure ozilFileExists
  Config ConfigInit   -> initAction (oDie alreadyExistsMsg) *> liftIO exitSuccess
  Config ConfigReInit -> initAction unreachableErrorM *> pure OzilFileExists
  Config ConfigDelete -> unreachableError
  Config ConfigSync   -> promptInit
 where
  promptInit = case ozilFileExists of
    OzilFileExists -> pure OzilFileExists
    OzilFileMissing -> do
      create <- liftIO $ prompt DefaultYes promptMsg
      when create (initAction unreachableError)
      pure (create ^. exists)
  promptMsg = "Configuration file not found. Should I initialize one?"
  initAction :: (forall a. Startup a) -> Startup ()
  initAction death = case ozilFileExists of
    OzilFileExists  -> death
    OzilFileMissing -> do
      effcp <- effectiveConfigPath
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory effcp)
        BS.writeFile effcp
          $ (encode . view Conf.userConfig) Default.config
      modifyConfig (set Conf.configFileExists (Just effcp))
  alreadyExistsMsg
    = "Error: configuration file already exists. \
      \Maybe you wanted to use ozil config reinit?"

readWriteConfig :: HasCallStack => OzilFileExists -> Startup ()
readWriteConfig = \case
  OzilFileMissing -> error "TODO: Decide the appropriate behavior here."
  OzilFileExists  -> view optCommand >>= \case
    Config ConfigSync   -> readConfig *> syncConfig *> liftIO exitSuccess
    Config ConfigReInit -> readConfig *> syncConfig *> liftIO exitSuccess
    Config ConfigDelete -> pure ()
    Config ConfigInit   -> pure ()
    Default{}           -> pure ()
    WhatIs{}            -> pure ()
 where
  -- TODO: Implement this.
  syncConfig = pure ()
  readConfig = do
    effcp <- effectiveConfigPath
    liftIO (decodeFileEither effcp) >>= \case
      Right cfg -> modifyConfig (set Conf.userConfig cfg)
      Left err  ->
        liftIO . warn . configDecodeWarning $ prettyPrintParseException err
  configDecodeWarning s = T.pack $
    printf "Couldn't parse the config file %s.\n%s" Default.configPath s

checkDbExists :: Startup Bool
checkDbExists = pure True
-- checkDbExists = do
--   p <- L.use (Conf.userConfig . Conf.databasePath)
--   (Conf.systemInfo . Conf.ozilDbExists) <~= liftIO (doesFileExist p)

syncDbIfApplicable :: Bool -> Startup ()
syncDbIfApplicable _ = pure ()

saveConfig :: HasCallStack => Startup a
saveConfig = unimplementedErrorM
