{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Ozil.App.Cmd.Types where

import Control.Lens.TH (makeFields, makePrisms)

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

data InputFileType
  = Binary
  | ManPage { _zipped :: !Bool }
  deriving Show

data InputFile = InputFile
  { _inputFileFileType :: !InputFileType
  , _inputFilePath :: !FilePath
  } deriving Show

data DefaultOptions = DefaultOptions
  { _defaultOptionsAutofind :: !Bool
  , _defaultOptionsInputs :: ![InputFile]
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
  -- | Db !DbOptions
  | Default !DefaultOptions
  | WhatIs !WhatIsOptions
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
