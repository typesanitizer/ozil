module System.Man where

import Data.List (union)
import Data.String (IsString)
import System.Environment (getEnv)

import qualified Data.Text as T

defaultManPath :: IsString a => a
defaultManPath = "/usr/share/man"

-- TODO: Replace this with a vector.
manPath :: IO [T.Text]
manPath = do
  envPaths <- T.split (== ':') . T.pack <$> getEnv "MANPATH"
  pure $ union [defaultManPath] envPaths
