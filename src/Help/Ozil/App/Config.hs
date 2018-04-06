module Help.Ozil.App.Config where

import Data.Text (Text)
import Data.Vector.Unboxed as VU

newtype Config = Config
  { helpByDefault :: VU.Vector Text
  }
