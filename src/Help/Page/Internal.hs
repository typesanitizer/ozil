module Help.Page.Internal where

import Commons (Generic, Text)

import Help.Page.Help (HelpPage)
import Help.Page.Man (ManPage (..), WhatisDescription (..))
import Help.Subcommand (Subcommand)

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

data DocPage
  = Man  { _docPageManSummary  :: ManPageSummary, _docPageManPage  :: ManPage }
  | Help { _docPageHelpSummary :: HelpPageSummary, _docPageHelpPage :: HelpPage }

instance Eq DocPage where
  (==) Man{}  Help{} = False
  (==) Help{} Man{} = False
  (==) (Man ms1 _) (Man ms2 _) = ms1 == ms2
  (==) (Help hs1 _) (Help hs2 _) = hs1 == hs2

type ManPageSummary = WhatisDescription

-- FIXME: Erm, these record field names don't make sense. Available and Text?
data HelpPageSummary = HelpPageSummary
  { _binaryPath         :: BinaryPath
  , _subcommandPath     :: [Subcommand]
  , _shortHelpAvailable :: !Bool
  , _shortHelpText      :: Text
  } deriving Eq

data BinaryPath
  = Simple FilePath
  | Local BuildSystem BinName
  deriving (Eq, Show, Generic)

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

type BinName = String

data DocPageSummary
  = ManSummary  !ManPageSummary
  | HelpSummary !HelpPageSummary

data LinkState
  = LinksOff {len :: !Int, _prev :: !Int}
  | LinksOn  {len :: !Int, _sel  :: !Int}
  deriving Show
