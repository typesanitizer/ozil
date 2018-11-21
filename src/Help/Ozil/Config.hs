{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Help.Ozil.Config
  ( getConfig
  , saveConfig
  , module Help.Ozil.Config.Watch
  , Conf.Config
  , getConfigSimple
  , ParseException
  )
  where

import Commons

import Help.Ozil.Config.Watch
import Help.Ozil.Death

import Help.Ozil.Cmd (configPath, optCommand, Command (..), ConfigOptions (..))
import Help.Ozil.Config.Types (HasKeyBindings (..))
import Help.Ozil.Console.Text (warn, prompt, pattern DefaultYes)
import Help.Ozil.Startup.Core (options, modifyConfig, Startup)

import System.Directory

import Data.Yaml (ParseException, prettyPrintParseException, decodeFileEither, encode)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Help.Ozil.Config.Types as Conf
import qualified Help.Ozil.Config.Default as Default

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

{-# ANN getConfigSimple ("HLint: ignore Avoid lambda" :: String) #-}
-- | Returns a function that uses everything from the newly read config,
-- except for one thing -- the new config might only have incomplete
-- keybindings, so you provide a backup config as an argument to provide
-- bindings missing from the newly read config.
getConfigSimple
  :: FilePath
  -> IO (Either ParseException (Conf.Config -> Conf.Config))
getConfigSimple p = decodeFileEither p
  <&> fmap (\cfg -> over Conf.userConfig (mergeConfig cfg))
  where
    mergeConfig inp sofar = let kbs = keyBindings in
      over kbs (H.unionWith (flip const) (sofar ^. kbs)) inp

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
  pure (coerce (isJust cfe))
  where
    errConfigFileMissing :: FilePath -> a
    errConfigFileMissing x = error
      $ printf "Error: Expected a config file at %s but it wasn't found.\n\
               \Perhaps double-check the path?" x

effectiveConfigPath :: Startup FilePath
effectiveConfigPath =
  fromMaybe Default.configPath <$> view (options . configPath)

newtype OzilFileExists = MkOzilFileExists Bool

{-# COMPLETE OzilFileExists, OzilFileMissing #-}
pattern OzilFileMissing, OzilFileExists :: OzilFileExists
pattern OzilFileMissing = MkOzilFileExists False
pattern OzilFileExists  = MkOzilFileExists True

deleteConfigFileIfApplicable :: OzilFileExists -> Startup OzilFileExists
deleteConfigFileIfApplicable ozilFileExists = view optCommand >>= \case
  Config ConfigDelete -> delete *> liftIO exitSuccess
  Config ConfigReInit -> delete *> pure OzilFileMissing
  Config ConfigSync   -> pure ozilFileExists
  Config ConfigInit   -> pure ozilFileExists
  WhatIs _            -> pure ozilFileExists
  Default _           -> pure ozilFileExists
 where
  delete = when (coerce ozilFileExists) $ do
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
      pure (coerce create)
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

readWriteConfig :: OzilFileExists -> Startup ()
readWriteConfig = \case
  OzilFileMissing -> pure ()
  OzilFileExists  -> view optCommand >>= \case
    Config ConfigSync   -> readConfig *> syncConfig *> liftIO exitSuccess
    Config ConfigReInit -> readConfig *> syncConfig *> liftIO exitSuccess
    Config ConfigDelete -> pure ()
    Config ConfigInit   -> pure ()
    Default{}           -> readConfig
    WhatIs{}            -> pure ()
 where
  -- TODO: Implement this.
  syncConfig = pure ()
  readConfig = do
    effcp <- effectiveConfigPath
    mod_f <- liftIO $ getConfigSimple effcp
    case mod_f of
      Right f -> modifyConfig f
      Left err ->
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
