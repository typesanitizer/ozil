module Development.BuildSystem
  ( BuildSystem
  , mkExecArgs
  , buildSystems
  , checkTillRoot
  , getHelpTextViaBS
  ) where

import Commons (maybeToList, readProcessSimple, headMaybe)

import Help.Subcommand

import Data.Aeson

import Data.Aeson.Types (typeMismatch)
import Data.List (inits)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.FilePath (joinPath, (</>), splitDirectories)

import qualified Data.Text as T

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

getHelpTextViaBS :: BuildSystem -> String -> [Subcommand] -> [String] -> IO (Maybe T.Text)
getHelpTextViaBS bs bin subcs args = case bs of
  Stack -> do
    binpath <- stackFindBinary bin
    case binpath of
      Nothing -> pure Nothing
      Just p -> readProcessSimple p args'
  Cabal -> readProcessSimple "cabal" (["v2-exec", bin, "--"] <> args')
  Cargo -> readProcessSimple "cargo" (["run", "--bin", bin, "--"] <> args')
  where
    args' = map show subcs <> args

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

stackFindBinary :: String -> IO (Maybe FilePath)
stackFindBinary binName = do
  stack_path_m <- readProcessSimple "stack" ["path"]
  let search_root = do
        stack_paths <- T.lines <$> stack_path_m
        let getPath s = headMaybe . map (T.strip . T.drop (T.length s))
              $ filter (T.isPrefixOf s) stack_paths
        proj_root <- getPath "project-root:"
        dist_dir <- getPath "dist-dir:"
        pure (T.unpack proj_root </> T.unpack dist_dir)
  go (maybeToList search_root)
  where
    go [] = pure Nothing
    go (x:xs) = do
      isDir <- doesDirectoryExist x
      if isDir then do
        gotcha <- doesFileExist (x </> binName)
        if gotcha then pure (Just (x </> binName))
        else (\ys -> go (map (x </>) ys ++ xs)) =<< listDirectory x
      else go xs
