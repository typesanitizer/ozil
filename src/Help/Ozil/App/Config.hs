module Help.Ozil.App.Config where

import Data.Set (Set)
import Data.Text (Text)

data Config = Config
  { helpByDefault :: Set Text
  , databasePath :: FilePath
  }
