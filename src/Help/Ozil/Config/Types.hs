{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.Config.Types
  ( module Help.Ozil.Config.Types
  , mkChoice
  , getPagePath
  , KeyBindings
  ) where

import Commons
import Help.Ozil.Config.Types.Internal

import Lens.Micro (Lens')
import Lens.Micro.TH (makeLenses)

type V = 'V1_0

type SystemInfo = SystemInfoF V
type Choice = ChoiceF V
type UserConfig = UserConfigF V
type Config = ConfigF V

makeLenses ''SystemInfoV1_0
makeFieldsNoPrefix ''ChoiceV1_0
makeFieldsNoPrefix ''UserConfigV1_0
makeLenses ''ConfigV1_0

configFileExists :: Lens' Config (Maybe FilePath)
configFileExists = systemInfo . ozilConfigFileExists
