{-# LANGUAGE MultiWayIf #-}

module Help.Ozil.App where

import Help.Page
import Help.Ozil.App.Core
import Help.Ozil.App.Cmd
import Help.Ozil.App.Console.Text
import System.Directory

import Control.Monad (when, void, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (put)
import Data.Yaml (prettyPrintParseException, decodeFileEither, encode)
import System.Exit (exitSuccess, die)
import Text.Printf (printf)

import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Help.Ozil.App.Default as Default

main :: IO ()
main = defaultMain runOzil

runOzil :: Options -> IO ()
runOzil opts = void . join $ execO opts <$> Default.config <*> pure
  (getConfig >> selectPages >>= viewPages >> saveConfig)

getConfig :: O ()
getConfig =
  liftIO foundConfigDir
    >>= deleteConfigDirIfApplicable
    >>= createConfigFileIfApplicable
    >>  syncDbIfApplicable
    >>= readConfig

foundConfigDir :: IO Bool
foundConfigDir = doesDirectoryExist =<< Default.configDir

deleteConfigDirIfApplicable :: Bool -> O Bool
deleteConfigDirIfApplicable configDirExists =
  L.view optCommand >>= liftIO . \case
    Config ConfigDelete -> delete >> exitSuccess
    Config ConfigReInit -> delete >> pure False
    _                   -> pure configDirExists
 where
  delete = when configDirExists . removePathForcibly =<< Default.configFilePath

createConfigFileIfApplicable :: Bool -> O ()
createConfigFileIfApplicable configDirExists = L.view optCommand >>= \case
  Config ConfigInit   -> initAction
  Config ConfigReInit -> initAction
  _                   -> liftIO configFileExists >>= \case
    False -> do
      create <- liftIO $ prompt True promptMsg
      liftIO $ print create
      when create initAction
    True -> liftIO (decodeFileEither =<< Default.configFilePath) >>= \case
      Right cfg -> put cfg
      Left err ->
        liftIO $ warn =<< configDecodeWarning (prettyPrintParseException err)
 where
  configFileExists =
    (configDirExists &&) <$> (doesFileExist =<< Default.configFilePath)
  initAction = liftIO $ configFileExists >>= \case
    True  -> die alreadyExistsMsg
    False -> do
      createDirectoryIfMissing True =<< Default.configDir
      join
        $   BS.writeFile
        <$> Default.configFilePath
        <*> fmap encode Default.config
      exitSuccess
  alreadyExistsMsg
    = "Error: configuration file already exists. \
      \Maybe you wanted to use ozil config reinit?"
  promptMsg = "Configuration directory not found. Should I initialize one?"
  configDecodeWarning :: String -> IO T.Text
  configDecodeWarning s =
    T.pack
      <$> (   printf "Couldn't parse the config file %s.\n%s"
          <$> Default.configFilePath
          <*> pure s
          )

syncDbIfApplicable :: a
syncDbIfApplicable = undefined

readConfig :: a
readConfig = undefined

saveConfig :: O ()
saveConfig = undefined

selectPages :: O [DocPage]
selectPages = undefined
-- selectPage opts = do
--   print opts
--   exitSuccess
  -- manPages <- getManPages opts
  -- helpPage <- getHelpPage opts
  -- userSelection . catMaybes $ helpPage : manPages

-- getManPages :: Options -> IO DocPage
getManPages :: a
getManPages = undefined

getHelpPage :: a
getHelpPage = undefined

userSelection :: a
userSelection = undefined

viewPages :: [DocPage] -> O ()
viewPages = undefined
-- viewPage :: Options -> DocPage -> IO ()
-- viewPage = undefined
