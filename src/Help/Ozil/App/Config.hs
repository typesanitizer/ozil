{-# LANGUAGE RankNTypes #-}

module Help.Ozil.App.Config
  ( getConfig
  , saveConfig
  , module Help.Ozil.App.Config.Watch
  )
  where

import Commons

import Help.Ozil.App.Cmd (optCommand, Command (..), ConfigOptions (..))
import Help.Ozil.App.Config.Watch
import Help.Ozil.App.Console.Text (warn, prompt)
import Help.Ozil.App.Death (unreachableError, unreachableErrorM, oDie)
import Help.Ozil.App.Startup (modifyConfig, Startup)
import System.Directory

import Data.Yaml (prettyPrintParseException, decodeFileEither, encode)
import System.Exit (exitSuccess)
import Text.Printf (printf)

import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Help.Ozil.App.Config.Types as Conf
import qualified Help.Ozil.App.Default as Default

getConfig :: Startup ()
getConfig =
  foundConfigFile
    >>= deleteConfigFileIfApplicable
    >>= createConfigFileIfApplicable
    >>= readWriteConfig
    >>  checkDbExists
    >>= syncDbIfApplicable

foundConfigFile :: Startup OzilFileExists
foundConfigFile = do
  b <- liftIO (doesFileExist Default.configPath)
  modifyConfig (set Conf.configFileExists b)
  pure (view exists b)

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
    modifyConfig (set Conf.configFileExists False)

createConfigFileIfApplicable :: OzilFileExists -> Startup OzilFileExists
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
      create <- liftIO $ prompt True promptMsg
      when create (initAction unreachableError)
      pure (create ^. exists)
  initAction :: (forall a. Startup a) -> Startup ()
  initAction death = case ozilFileExists of
    OzilFileExists  -> death
    OzilFileMissing -> do
      liftIO $ do
        createDirectoryIfMissing True Default.configDir
        BS.writeFile Default.configPath
          $ (encode . view Conf.userConfig) Default.config
      modifyConfig (set Conf.configFileExists True)
  alreadyExistsMsg
    = "Error: configuration file already exists. \
      \Maybe you wanted to use ozil config reinit?"
  promptMsg = "Configuration file not found. Should I initialize one?"

readWriteConfig :: OzilFileExists -> Startup ()
readWriteConfig = \case
  OzilFileMissing -> undefined
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
  readConfig = liftIO (decodeFileEither Default.configPath) >>= \case
    Right cfg -> modifyConfig (set Conf.userConfig cfg)
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

saveConfig :: Startup ()
saveConfig = undefined
