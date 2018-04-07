{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.App.Config where

import Data.Set (Set)
import Data.Text (Text)

import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))

data SystemInfo = SystemInfo
  { _ozilConfigDirExists :: Bool
  , _ozilConfigFileExists :: Bool
  , _ozilDbExists :: Bool
  } deriving Show

data UserConfig = UserConfig
  { _helpByDefault :: Set Text
  , _databasePath :: FilePath
  } deriving Show

data Config = Config
  { _userConfig :: UserConfig
  , _systemInfo :: SystemInfo
  } deriving Show

makeLenses ''SystemInfo
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''UserConfig)
makeLenses ''UserConfig
makeLenses ''Config

configDirExists :: Lens' Config Bool
configDirExists = systemInfo . ozilConfigDirExists

configFileExists :: Lens' Config Bool
configFileExists = systemInfo . ozilConfigFileExists
