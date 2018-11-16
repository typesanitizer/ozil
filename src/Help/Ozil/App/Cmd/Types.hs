{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Ozil.App.Cmd.Types
  ( module Help.Ozil.App.Cmd.Types
  ) where

import Commons

import Lens.Micro (Traversal')

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

newtype Zipped = MkZipped Bool

{-# COMPLETE Unzipped, Zipped #-}
pattern Unzipped :: Zipped
pattern Unzipped = MkZipped False

pattern Zipped :: Zipped
pattern Zipped = MkZipped True

instance Show Zipped where
  show = \case
    Unzipped -> "Unzipped"
    Zipped -> "Zipped"

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

-- | Unit corresponding to one help/man "item".
--
-- For example, if one types @ozil 'rustup override set'@ then, primary
-- would correspond to @rustup@ and subcommandPath would be
-- @["override", "set"]@.
data CmdInput = CmdInput
  { _cmdInputPrimary        :: !InputFile
  , _cmdInputSubcommandPath :: [String]
  } deriving Show

data DefaultOptions = DefaultOptions
  { _defaultOptionsAutofind  :: !Bool
  , _defaultOptionsInputs    :: !CmdInput
  , _defaultOptionsDebugMode :: !Bool
  } deriving Show

data Query = QueryDefault | QueryFull
  deriving Show

type RegexStr = String

data WhatIsOptions = WhatIsOptions
  { _whatIsOptionsQuery :: Maybe Query
  , _whatIsOptionsInputs :: RegexStr
  } deriving Show

-- | Subcommand under use.
data Command
  = Config  !ConfigOptions
  | Default !DefaultOptions -- ^ Default ==> run viewer
  | WhatIs  !WhatIsOptions   -- ^ WhatIs equivalent
  -- | Db !DbOptions
  deriving Show

-- | A "Prism" to check if the value is _Default.
--
-- microlens doesn't have Prisms so we make do with Traversal'.
_Default :: Traversal' Command DefaultOptions
_Default f cmd = case cmd of
  Config _  -> pure cmd
  Default dopt -> Default <$> f dopt
  WhatIs _  -> pure cmd

-- | Command line options
data Options = Options
  { _optionsConfigPath :: !(Maybe FilePath)
  , _optionsOptCommand :: !Command
  } deriving Show

makeFields ''InputFile
makeFields ''CmdInput
makeFields ''DefaultOptions
makeFields ''WhatIsOptions
makeFields ''Options
