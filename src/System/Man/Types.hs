module System.Man.Types where

import Data.Map
import Data.Vector as V

data ManpathConfig = ManpathConfig
  { _mandatoryManpath :: V.Vector FilePath
  , _manpathMap :: Map FilePath FilePath
  }
