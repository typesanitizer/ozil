{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.App.Config.Types where

import Commons

import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))
import Lens.Micro (Lens')
import Lens.Micro.TH (makeLenses)

data SystemInfo = SystemInfo
  { _ozilConfigFileExists :: !(Maybe FilePath)
  , _ozilDbExists :: () -- ^ Unused for now
  } deriving Show

data UserConfig = UserConfig
  { _helpByDefault :: Set Text
  , _databasePath :: () -- ^ Unused for now
  } deriving Show

data Config = Config
  { _userConfig :: !UserConfig
  , _systemInfo :: !SystemInfo
  } deriving Show

makeLenses ''SystemInfo
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''UserConfig)
makeLenses ''UserConfig
makeLenses ''Config

configFileExists :: Lens' Config (Maybe FilePath)
configFileExists = systemInfo . ozilConfigFileExists
