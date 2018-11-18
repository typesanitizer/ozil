{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.App.Config.Types where

import Commons
import Help.Page (BinaryPath)

import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))
import Lens.Micro (Lens')
import Lens.Micro.TH (makeLenses)

data SystemInfo = SystemInfo
  { _ozilConfigFileExists :: !(Maybe FilePath)
  , _ozilDbExists         :: () -- ^ Unused for now
  } deriving Show

data Choice = Choice
  { _options :: NonEmpty BinaryPath
  , _choice  :: Int
  } deriving Show

data UserConfig = UserConfig
  { _savedSelection :: HashMap Text Choice
  , _databasePath   :: () -- ^ Unused for now
  } deriving Show

data Config = Config
  { _userConfig :: !UserConfig
  , _systemInfo :: !SystemInfo
  } deriving Show

makeLenses ''SystemInfo
makeLenses ''Choice
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Choice)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''UserConfig)
makeLenses ''UserConfig
makeLenses ''Config

configFileExists :: Lens' Config (Maybe FilePath)
configFileExists = systemInfo . ozilConfigFileExists
