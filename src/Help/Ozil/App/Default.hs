{-| Default settings for everything.

This module should be imported qualified.
-}
module Help.Ozil.App.Default where

import Help.Ozil.App.Config

import Data.Set (empty)
import Data.String (IsString)
import System.FilePath ((</>))

configDir :: IsString a => a
configDir = "~/.ozil"

configFile :: IsString a => a
configFile = "ozil.yaml"

configFilePath :: FilePath
configFilePath = configDir </> configFile

dbFile :: IsString a => a
dbFile = "ozil_docpages.sqlite"

dbFilePath :: FilePath
dbFilePath = configDir </> dbFile

config :: Config
config = Config
  { helpByDefault = empty
  , databasePath = dbFilePath
  }
