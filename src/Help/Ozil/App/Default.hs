{-| Default settings for everything.

This module should be imported qualified.
-}
module Help.Ozil.App.Default where

import Help.Ozil.App.Config.Types

import Data.String (IsString)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashMap.Strict as H

-- unsafePerformIO might give wrong results if the person changes
-- between users in the same session but that seems fairly unlikely
-- (can you even do that?).
configDir :: FilePath
configDir = unsafePerformIO getHomeDirectory </> ".config" </> "ozil"
{-# NOINLINE configDir #-}

displayConfigDir :: IsString a => a
displayConfigDir = "~/.config/ozil"

configFileName :: IsString a => a
configFileName = "ozil.yaml"

configPath :: FilePath
configPath = configDir </> configFileName

displayConfigFilePath :: FilePath
displayConfigFilePath = displayConfigDir </> configFileName

dbFile :: IsString a => a
dbFile = "ozil_docpages.sqlite"

dbFilePath :: FilePath
dbFilePath = configDir </> dbFile

displayDbFilePath :: FilePath
displayDbFilePath = displayConfigDir </> dbFile

config :: Config
config =
  Config
    { _systemInfo = SystemInfo
      { _ozilConfigFileExists = Nothing
      , _ozilDbExists = ()
      }
    , _userConfig = UserConfig
      { _savedSelection = H.empty
      , _databasePath = ()
      }
    }
