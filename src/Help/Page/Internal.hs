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
  } deriving (Eq, Show)

data BinaryPath
  = Global FilePath
  | Local FilePath BuildSystem BinName -- ^ The FilePath is the project folder.
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
  deriving Show

data LinkState
  = LinksOff {len :: !Int, _prev :: !Int}
  | LinksOn  {len :: !Int, _sel  :: !Int}
  deriving Show

data PagePath
  = ManPath  { _fullName :: Text , _section :: Text }
  | HelpPath BinaryPath
  deriving Show

instance ToJSON PagePath where
  toJSON = \case
    ManPath t s -> object
      [ "kind" .= ("man" :: Text)
      , "full-name" .= t
      , "section" .= s
      ]
    HelpPath b -> case b of
      Global p -> object [ "kind" .= ("help" :: Text)
                         , "help-root" .= ("global" :: Text)
                         , "path" .= p
                         ]
      Local pf bs bn -> object
        [ "kind" .= ("help" :: Text)
        , "help-root" .= ("local" :: Text)
        , "path" .= pf
        , "build-system" .= bs
        , "binary-name" .= bn
        ]

instance FromJSON PagePath where
  parseJSON (Object o) = do
    k <- (o .: "kind") :: Parser Text
    case k of
      "man" -> ManPath <$> o .: "full-name" <*> o .: "section"
      "help" -> do
        style <- (o .: "help-root") :: Parser Text
        case style of
          "global" -> HelpPath . Global <$> o .: "path"
          "local" -> fmap HelpPath $ Local
                     <$> o .: "path"
                     <*> o .: "build-system"
                     <*> o .: "binary-name"
          _ -> fail "help-root should be global or local"
      _ -> fail "kind should be man or help."
  parseJSON invalid = typeMismatch "PagePath" invalid
