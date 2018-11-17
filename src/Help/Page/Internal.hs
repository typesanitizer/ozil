module Help.Page.Internal where

import Commons (Text)

import Help.Page.Help (HelpPage)
import Help.Page.Man (ManPage (..), WhatisDescription (..))
import Help.Subcommand (Subcommand)

data DocPage
  = Man  { _docPageManSummary  :: ManPageSummary, _docPageManPage  :: ManPage }
  | Help { _docPageHelpSummary :: HelpPageSummary, _docPageHelpPage :: HelpPage }

instance Eq DocPage where
  (==) Man{}  Help{} = False
  (==) Help{} Man{} = False
  (==) (Man ms1 _) (Man ms2 _) = ms1 == ms2
  (==) (Help hs1 _) (Help hs2 _) = hs1 == hs2

data ManPageSummary
  = WhatisDescr !WhatisDescription
  | UnknownFormat { _shortDescription :: String }
  deriving Eq

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
  deriving Eq

data BuildSystem = Stack | Cabal | Cargo
  deriving Eq

type BinName = String

data DocPageSummary
  = ManSummary  !ManPageSummary
  | HelpSummary !HelpPageSummary

data LinkState
  = LinksOff {len :: !Int, _prev :: !Int}
  | LinksOn  {len :: !Int, _sel  :: !Int}
  deriving Show
