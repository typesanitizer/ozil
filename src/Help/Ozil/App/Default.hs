{-| Default settings for everything.

This module should be imported qualified.
-}
module Help.Ozil.App.Default where

import Help.Ozil.App.Config

import Control.Lens ((<&>))
import Data.Set (empty)
import Data.String (IsString)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

configDir :: IO FilePath
configDir = getHomeDirectory <&> (</> ".ozil")

displayConfigDir :: IsString a => a
displayConfigDir = "~/.ozil"

configFile :: IsString a => a
configFile = "ozil.yaml"

configFilePath :: IO FilePath
configFilePath = configDir <&> (</> configFile)

displayConfigFilePath :: FilePath
displayConfigFilePath = displayConfigDir </> configFile

dbFile :: IsString a => a
dbFile = "ozil_docpages.sqlite"

dbFilePath :: IO FilePath
dbFilePath = configDir <&> (</> dbFile)

displayDbFilePath :: FilePath
displayDbFilePath = displayConfigDir </> dbFile

config :: IO Config
config = do
  p <- dbFilePath
  pure Config
    { helpByDefault = empty
    , databasePath = p
    }
