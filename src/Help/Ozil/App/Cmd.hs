{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Ozil.App.Cmd
  -- ( Options (..) -- TODO: Avoid exporting constructors.
  -- , options
  -- , defaultMain
  -- )
  where

import Options.Applicative

import Control.Lens.TH (makeFields)
import Data.Semigroup ((<>))
import System.FilePath (takeExtension)

import qualified Help.Ozil.App.Default as Default

-- * Runner

defaultMain :: (Options -> IO b) -> IO b
defaultMain runOzil = execParser opts >>= runOzil
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> header "ozil - Frictionless browsing of man/help pages."
    <> progDesc
         "ozil assists you with viewing man/help pages. It is intended as a replacement to man/--help + less/more/most."
    )

-- * Types

data ConfigOptions
  = ConfigInit   -- ^ Initialize a config file.
  | ConfigDelete -- ^ Delete the config file.
  | ConfigReInit -- ^ Delete then init then sync.
  | ConfigSync   -- ^ Sync with manpath.config
  deriving Show

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

data CommonOptions = CommonOptions
  { _commonOptionsAutofind :: !Bool
  , _commonOptionsInputs :: ![InputFile]
  } deriving Show

type DefaultOptions = CommonOptions

data Query = QueryDefault | QueryFull
  deriving Show

data WhatIsOptions = WhatIsOptions
  { _whatIsOptionsQuery :: Maybe Query
  , _whatisOptionsCommon :: CommonOptions
  } deriving Show

data Command
  = Config !ConfigOptions
  | Db !DbOptions
  | Default !DefaultOptions
  | WhatIs !WhatIsOptions
  deriving Show

data Options = Options
  { _optionsOptCommand :: !Command
  , _optionsConfigPath :: !(Maybe FilePath)
  } deriving Show

-- * Parsers

configOptionsP = hsubparser
   (  command "init"
      (info
       (pure ConfigInit)
       (progDesc "Initialize a configuration file."))
   <> command "delete"
      (info
       (pure ConfigDelete)
       (progDesc "Delete the configuration file."))
   <> command "reinit"
      (info
       (pure ConfigReInit)
       (progDesc "Alias for ozil config delete \
                 \&& ozil config init \
                 \&& ozil config sync."))
   <> command "sync"
       (info
        (pure ConfigSync)
        (progDesc
         $ "Sync " ++ Default.configFile ++ " with /etc/manpath.config."))
   )

configPathP :: Parser (Maybe FilePath)
configPathP
  = option auto
  $ long "config"
  <> short 'c'
  <> help "Path to config file (default: .ozil.yaml)."
  <> metavar "PATH"

options :: Parser Options
options = Options
  <$> hsubparser
  (command "config"
   (info (Config <$> configOptionsP)
    (progDesc
     $ "Tweak configuration [default: " ++ Default.configFilePath ++ "]."))
  )
  <*> configPathP
 where
  offSwitch = fmap not . switch
  filesP    = some (toInputFile <$> strArgument (metavar "<files>"))
  toInputFile s = InputFile filetype s
   where
    ext      = takeExtension s
    filetype = case ext of
      "" -> Binary
      _  -> ManPage (ext == ".gz")

makeFields ''InputFile
makeFields ''CommonOptions
makeFields ''WhatIsOptions
makeFields ''Options
