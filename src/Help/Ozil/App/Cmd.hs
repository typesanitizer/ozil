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
         "ozil assists you with viewing man/help pages. \
         \It is intended as a replacement for man/--help + \
         \less/more/most."
    )

-- * Types

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
  -- | Db !DbOptions
  | Default !DefaultOptions
  | WhatIs !WhatIsOptions
  deriving Show

data Options = Options
  { _optionsConfigPath :: !(Maybe FilePath)
  , _optionsOptCommand :: !Command
  } deriving Show

-- * Parsers

configPathP :: Parser (Maybe FilePath)
configPathP
  = optional . option auto
  $ long "config"
  <> short 'c'
  <> help ("Path to config file [default: " ++ Default.configFilePath ++ "].")
  <> metavar "PATH"

configOptionsP :: Parser ConfigOptions
configOptionsP = subparser
   (  command "init"
      (info
       (pure ConfigInit)
       (progDesc "Initialize a config file."))
   <> command "delete"
      (info
       (pure ConfigDelete)
       (progDesc "Delete the config file."))
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

commonOptionsP :: Parser CommonOptions
commonOptionsP = CommonOptions
  <$>
  offSwitch
  (long "no-autofind"
   <> help "Don't try to be clever: only search for exact matches. \
           \Otherwise, ozil usually tries to be intelligent - \
           \if you ran 'ozil foo' inside a stack project and it failed, then \
           \ozil will automatically try 'ozil stack exec foo'.")
  <*> some (toInputFile <$> strArgument
            (metavar "<files>"
             <> help "Input: can be a binary name (e.g. gcc), or a \
                     \man page (e.g. gcc.1 or gcc.1.gz). If more \
                     \than one argument is given, the doc pages are \
                     \opened sequentially."))
  where
    offSwitch = fmap not . switch
    toInputFile s = InputFile filetype s
      where
        ext      = takeExtension s
        filetype = case ext of
          "" -> Binary
          _  -> ManPage (ext == ".gz")

whatIsOptionsP :: Parser WhatIsOptions
whatIsOptionsP = WhatIsOptions
  <$> queryP
  <*> commonOptionsP
  where
    queryP
      =   flag' (Just QueryDefault)
          (short 'q' <> long "query"
           <> help "Search sections Name and Description, like apropos(1).")
      <|> flag' (Just QueryFull)
          (long "query-full"
           <> help "Search man pages fully.")
      <|> pure Nothing

options :: Parser Options
options = Options
  <$> configPathP
  <*>
  ( Default <$> commonOptionsP
    <|> hsubparser
        (  configSubP
        <> whatIsSubP
        )
  )
  where
    configSubP =
      command "config"
      (info (Config <$> configOptionsP)
       . progDesc
       $ "Tweak config file [default: " ++ Default.configFilePath ++ "].")
    whatIsSubP =
     command "whatis"
      (info (WhatIs <$> whatIsOptionsP)
      . progDesc
       $ "Search man page sections [default: Names only, like whatis(1)]. \
         \The result is opened using ozil so you may follow pages using \
         \hints.")

makeFields ''InputFile
makeFields ''CommonOptions
makeFields ''WhatIsOptions
makeFields ''Options
