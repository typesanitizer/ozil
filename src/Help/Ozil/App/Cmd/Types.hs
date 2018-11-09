{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Ozil.App.Cmd.Types
  ( module Help.Ozil.App.Cmd.Types
  , NonEmpty (..)
  ) where

import Control.Lens (Iso', iso)
import Control.Lens.TH (makeFields, makePrisms)
import Data.List.NonEmpty (NonEmpty (..))

data ConfigOptions
  = ConfigInit   -- ^ Initialize a config file.
  | ConfigDelete -- ^ Delete the config file.
  | ConfigReInit -- ^ Delete then init then sync.
  | ConfigSync   -- ^ Sync with manpath.config
  deriving Show

-- Not entirely sure if we will need a database for faster indexing.
-- I suppose it depends on how many directories they have on their
-- man path, as well as if they're using an HDD vs an SSD.
-- We could do benchmarks later and see if we want to support this.
data DbOptions
  = DbInit
  | DbLocate
  | DbSync
  | DbDelete
  | DbReInit
  deriving Show

data Zipped = Unzipped | Zipped
  deriving Show

zipped :: Iso' Bool Zipped
zipped = iso
  (\case {False -> Unzipped; True -> Zipped;})
  (\case {Unzipped -> False; Zipped -> True;})

data InputFileType
  = Binary
  | ManPage { _zipped :: !Zipped }
  deriving Show

type FileName = FilePath

data InputFile
  = InputFile
    { _inputFileFileType :: !InputFileType
    , _inputFileFileName :: !FileName
    }
  | InputPath
    { _inputPathFileType :: !InputFileType
    , _inputFileFilePath :: !FilePath
    }
  deriving Show

data DefaultOptions = DefaultOptions
  { _defaultOptionsAutofind :: !Bool
  , _defaultOptionsInputs :: !(NonEmpty InputFile)
  } deriving Show

data Query = QueryDefault | QueryFull
  deriving Show

type RegexStr = String

data WhatIsOptions = WhatIsOptions
  { _whatIsOptionsQuery :: Maybe Query
  , _whatIsOptionsInputs :: RegexStr
  } deriving Show

data Command
  = Config !ConfigOptions
  | Default !DefaultOptions
  | WhatIs !WhatIsOptions
  -- | Db !DbOptions
  deriving Show

data Options = Options
  { _optionsConfigPath :: !(Maybe FilePath)
  , _optionsOptCommand :: !Command
  } deriving Show

makeFields ''InputFile
makeFields ''DefaultOptions
makeFields ''WhatIsOptions
makePrisms ''Command
makeFields ''Options
