module Help.Ozil.App.Cmd.Parser
  ( defaultMain
  ) where

import Help.Ozil.App.Cmd.Types

import Options.Applicative

import System.FilePath (isPathSeparator, takeExtension)
import Text.Printf (printf)

import qualified Control.Lens as L
import qualified Help.Ozil.App.Default as Default

-- | Top-level runner
defaultMain :: (Options -> IO b) -> IO b
defaultMain run = execParser opts >>= run
 where
  opts = info
    (helper <*> options)
    (  fullDesc
    <> header "ozil - Frictionless browsing of man/help pages."
    <> progDesc
         "ozil assists you with viewing man/help pages. \
         \It is intended as a replacement for man/--help + \
         \less/more/most."
    )

-- * Component parsers

configPathP :: Parser (Maybe FilePath)
configPathP =
  optional
    .  option auto
    $  long "config-path"
    <> short 'c'
    <> help
         (  "Path to config file [default: "
         ++ Default.displayConfigFilePath
         ++ "]."
         )
    <> metavar "PATH"

configOptionsP :: Parser ConfigOptions
configOptionsP = subparser
  (  command
      "init"
      (info (pure ConfigInit) (progDesc "Initialize the config directory."))
  <> command
       "delete"
       (info (pure ConfigDelete) (progDesc "Delete the config directory."))
  <> command
       "reinit"
       ( info
         (pure ConfigReInit)
         ( progDesc
           "Alias for \
           \ozil config delete \
           \&& ozil conf init \
           \&& ozil conf sync."
         )
       )
  <> command
       "sync"
       ( info
         (pure ConfigSync)
         (  progDesc
         $  "Sync "
         ++ Default.configFileName
         ++ " with /etc/manpath.config."
         )
       )
  )

defaultOptionsP :: Parser DefaultOptions
defaultOptionsP =
  DefaultOptions
    <$> offSwitch
          (  long "no-autofind"
          <> help
               "Don't try to be clever: only search for exact matches. \
               \Otherwise, ozil usually tries to be intelligent - \
               \if you ran 'ozil foo' inside a stack project and it failed, \
               \then ozil will automatically check project binaries for \
               \matches."
          )
    <*> ( toInputFile <$> strArgument
          (  metavar "<file>"
          <> help
             "Input: can be a binary name (e.g. gcc), or a man page \
             \(e.g. gcc.1 or gcc.1.gz) or a path (e.g. foo/a.out)."
          )
        )
    <*> switch
          (  long "debug"
          <> help "Run ozil in debug mode."
          )
 where
  offSwitch = fmap not . switch
  toInputFile s =
    if any isPathSeparator s
      then InputPath filetype s
      else InputFile filetype s
   where
    ext      = takeExtension s
    filetype = case ext of
      "" -> Binary
      _  -> ManPage (L.view zipped (ext == ".gz"))

-- TODO: Briefly explain syntax of POSIX regexes and give usage examples.
-- Also, add link to canonical resources on POSIX regexes (both online and
-- man page).
whatIsOptionsP :: Parser WhatIsOptions
whatIsOptionsP = WhatIsOptions <$> queryP <*> strArgument
  (metavar "<regexes>" <> help "POSIX regular expressions to search.")
 where
  queryP =
    flag'
        (Just QueryDefault)
        ( short 'q' <> long "query" <> help
          "Search sections Name and Description, like apropos(1)."
        )
      <|> flag' (Just QueryFull)
                (long "query-full" <> help "Search man pages fully.")
      <|> pure Nothing

options :: Parser Options
options =
  Options
    <$> configPathP
    <*> (hsubparser (configSubP <> whatIsSubP) <|> Default <$> defaultOptionsP)
 where
  configSubP = command
    "conf"
    ( info (Config <$> configOptionsP) . progDesc $ printf
      "Tweak configuration [%s]. Currently, there is only \
      \one configuration file [%s], but we may add a database in \
      \the future for faster lookup."
      (Default.displayConfigDir :: String)
      Default.displayConfigFilePath
    )
  whatIsSubP = command
    "wat"
    ( info (WhatIs <$> whatIsOptionsP)
    . progDesc
    $ "Search man page sections [default: Names only, like whatis(1)]. \
      \The result is opened using ozil so you may follow pages using hints."
    )
