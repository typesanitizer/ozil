{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Help.Ozil.App.Config.Types.Core where

import Help.Ozil.App.KeyBinding

import Data.Aeson

import Control.Monad (guard)
import Data.Aeson.Types (typeMismatch)
import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Help.Page (BinaryPath)

import qualified Data.List.NonEmpty as NE

data Version = V1_0

type family SystemInfoF (v :: Version) :: Type
type instance SystemInfoF 'V1_0 = SystemInfoV1_0

type family ChoiceF (v :: Version) :: Type
type instance ChoiceF 'V1_0 = ChoiceV1_0

type family UserConfigF (v :: Version) :: Type
type instance UserConfigF 'V1_0 = UserConfigV1_0

type family ConfigF (v :: Version) :: Type
type instance ConfigF 'V1_0 = ConfigV1_0

data ConfigV1_0 = ConfigV1_0
  { _userConfig :: !UserConfigV1_0
  , _systemInfo :: !SystemInfoV1_0
  } deriving Show

data ChoiceV1_0 = ChoiceV1_0
  { _options :: NonEmpty BinaryPath
  , _choice  :: Int
  } deriving Show

instance FromJSON ChoiceV1_0 where
  parseJSON (Object o) = do
    _options <- o .: "options"
    _choice  <- o .: "choice"
    guard (_choice < NE.length _options)
    pure ChoiceV1_0{_options, _choice}
  parseJSON invalid = typeMismatch "Choice" invalid

instance ToJSON ChoiceV1_0 where
  toJSON ChoiceV1_0{_options, _choice} =
    object ["options" .= _options, "choice" .= _choice]

type KeyBindings = HashMap Action (NonEmpty KeyBinding)

data UserConfigV1_0 = UserConfigV1_0
  { _savedPreferences :: HashMap Text ChoiceV1_0
  , _keyBindings      :: HashMap Action (NonEmpty KeyBinding)
  , _databasePath     :: () -- ^ Unused for now
  } deriving Show

instance FromJSON UserConfigV1_0 where
  parseJSON (Object o) = UserConfigV1_0
    <$> o .: "saved-preferences"
    <*> o .: "key-bindings"
    <*> pure ()
  parseJSON invalid = typeMismatch "User Config" invalid

instance ToJSON UserConfigV1_0 where
  toJSON UserConfigV1_0{_savedPreferences, _keyBindings} = object
    ["saved-selection" .= _savedPreferences, "key-bindings" .= _keyBindings]

data SystemInfoV1_0 = SystemInfoV1_0
  { _ozilConfigFileExists :: !(Maybe FilePath)
  , _ozilDbExists         :: () -- ^ Unused for now
  } deriving Show
