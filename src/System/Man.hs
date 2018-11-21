module System.Man where

import Commons

import Control.Exception (try)
import Data.List (union)
import Data.String (IsString)
import System.Environment (getEnv)
import System.IO.Error
import System.Man.Parser
import System.Man.Types

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Generic as V

defaultManPath :: IsString a => a
defaultManPath = "/usr/share/man"

manPath :: IO (Vector Text)
manPath = do
  envPaths <- T.split (== ':') . T.pack <$> getEnv "MANPATH"
  -- NOTE: The order of arguments in union is important, duplicates are
  -- removed only from the second list.
  pure . V.fromList $ union [defaultManPath] envPaths

manpathConfigPath :: IsString a => a
manpathConfigPath = "/etc/manpath.config"

manpathConfigText :: IO (Maybe Text)
manpathConfigText =
  try (T.readFile manpathConfigPath) <&> \case
    Right txt -> Just txt
    Left  err -> if
      | isDoesNotExistError err -> Nothing
      | isPermissionError err -> error
        $  "You don't have permission to access " ++ manpathConfigPath ++ shrug
      | isAlreadyInUseError err -> error
        $ manpathConfigPath ++ " is in use, can't access it" ++ shrug
      | otherwise -> error "Unknown IOError encountered."
  where
    shrug = " ¯\\_(ツ)_/¯"

manpathConfig :: IO (Maybe ManpathConfig)
manpathConfig = fmap parseManpathConfig <$> manpathConfigText
