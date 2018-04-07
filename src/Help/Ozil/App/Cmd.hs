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

data InputFileType
  = Binary
  | ManPage { _zipped :: !Bool }
  deriving Show

data InputFile = InputFile
  { _inputFileFileType :: !InputFileType
  , _inputFilePath :: !FilePath
  } deriving Show

data Options = Options
  { _optionsAutofind :: !Bool
  , _optionsConfigPath :: !(Maybe FilePath)
  , _optionsInputs :: ![InputFile]
  } deriving Show

-- * Parsers

options :: Parser Options
options =
  Options
    <$> offSwitch
          (  long "no-autofind"
          <> help "Turn off intelligent searching if binary is missing."
          )
    <*> option
          auto
          (  long "config"
          <> short 'c'
          <> help "Path to config file (default name: .ozil.yaml)."
          <> metavar "PATH"
          )
    <*> filesP
 where
  offSwitch = fmap not . switch
  filesP = some (toInputFile <$> strArgument (metavar "<files>"))
  toInputFile s = InputFile filetype s
    where
      ext = takeExtension s
      filetype = case ext of
          "" -> Binary
          _ -> ManPage (ext == ".gz")

makeFields ''InputFile
makeFields ''Options
