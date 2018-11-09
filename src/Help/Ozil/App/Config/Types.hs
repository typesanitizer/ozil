{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.App.Config.Types where

import Data.Set (Set)
import Data.Text (Text)

import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))

data SystemInfo = SystemInfo
  { _ozilConfigFileExists :: !Bool
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

configFileExists :: Lens' Config Bool
configFileExists = systemInfo . ozilConfigFileExists
