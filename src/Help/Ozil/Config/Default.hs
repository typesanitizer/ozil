{-| Default settings for everything.

This module should be imported qualified.
-}
module Help.Ozil.Config.Default where

import Help.Ozil.KeyBinding
import Help.Ozil.Config.Types
import Help.Ozil.Config.Types.Core

import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString)
import Graphics.Vty.Input (Key (..), Modifier (..))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List.NonEmpty as NE
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
  ConfigV1_0
    { _systemInfo = SystemInfoV1_0
      { _ozilConfigFileExists = Nothing
      , _ozilDbExists = ()
      }
    , _userConfig = UserConfigV1_0
      { _savedPreferences = H.empty
      , _keyBindings = defaultKeyBindings
      , _databasePath = ()
      }
    }

defaultKeyBindings :: H.HashMap Action (NonEmpty KeyBinding)
defaultKeyBindings = H.fromList
  . map (fmap (NE.fromList . map (uncurry mkBinding))) $
  [ (ScrollUp,           [(KChar 'k', []), (KUp,   [])])
  , (ScrollDown,         [(KChar 'j', []), (KDown, [])])
  , (ScrollUpHalfPage,   [(KChar 'u', [MCtrl])])
  , (ScrollDownHalfPage, [(KChar 'd', [MCtrl])])
  , (LinkFollow,         [(KChar 'n', [MCtrl]), (KEnter, [])])
  , (LinkGoBack,         [(KChar 'p', [MCtrl]), (KBS,    [])])
  , (LinkJumpNext,       [(KChar 'n', [])])
  , (LinkJumpPrevious,   [(KChar 'p', [])])
  , (ToggleLinks,        [(KChar 'f', [])])
  , (ExitProgram,        [(KEsc, []), (KChar 'q', [])])
  ]
