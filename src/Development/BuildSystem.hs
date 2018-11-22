module Development.BuildSystem
  ( BuildSystem
  , mkExecArgs
  , buildSystems
  , checkTillRoot
  ) where

import Help.Subcommand

import Data.Aeson

import Data.Aeson.Types (typeMismatch)
import Data.List (inits)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (joinPath, (</>), splitDirectories)

data BuildSystem = Cabal | Cargo | Stack
  deriving (Eq, Show, Generic)

instance ToJSON BuildSystem where
  toJSON = \case
    Stack -> String "stack"
    Cabal -> String "cabal"
    Cargo -> String "cargo"

instance FromJSON BuildSystem where
  parseJSON (String s) = case s of
    "cabal" -> pure Cabal
    "cargo" -> pure Cargo
    "stack" -> pure Stack
    _ -> fail "Unrecognized build system."
  parseJSON invalid = typeMismatch "Build system" invalid

mkExecArgs :: BuildSystem -> String -> [Subcommand] -> (String, [String])
mkExecArgs bs bin subcs = case bs of
  Stack -> ("stack", ["exec", bin, "--"] <> map show subcs)
  Cabal -> ("cabal", ["v2-exec", bin, "--"] <> map show subcs)
  Cargo -> ("cargo", ["run", "--bin", bin, "--"] <> map show subcs)

buildSystems :: [BuildSystem]
buildSystems = [Stack, Cabal, Cargo]

checkTillRoot :: BuildSystem -> IO Bool
checkTillRoot bs = do
  let s = case bs of
        Cabal -> "cabal.project"
        Cargo -> "Cargo.toml"
        Stack -> "stack.yaml"
  cwd <- getCurrentDirectory
  let dirs = splitDirectories cwd
  fexists <- traverse (doesFileExist . (</> s) . joinPath) $ reverse (inits dirs)
  pure (or fexists)
