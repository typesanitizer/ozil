module Help.Page where

import Help.Page.Man
import Help.Page.Help

import Data.Text (Text)

--------------------------------------------------------------------------------
-- * Pages

data DocPage
  = Man { _manPage :: ManPage }
  | LongHelp { _longHelpPage :: HelpPage }
  | ShortHelp { _shortHelpPage :: HelpPage }

newtype ManPageSummary = ManPageSummary { shortDescription :: String }

data HelpPageSummary = HelpPageSummary
  { binaryPath         :: FilePath
  , shortHelpAvailable :: !Bool
  , shortHelpText      :: Text
  }

data DocPageSummary
  = ManSummary  !ManPageSummary
  | HelpSummary !HelpPageSummary

-- TODO: Improve this...
parseLongHelp :: Text -> HelpPage
parseLongHelp txt = HelpPage
  { _helpPageHeading = Nothing
  , _helpPageSynopsis = Nothing
  , _helpPageRest = txt
  }

-- TODO: Improve this...
parseShortHelp :: Text -> HelpPage
parseShortHelp = parseLongHelp

parseMan :: Text -> ManPage
parseMan txt = emptyManPage { _manPageRest = txt }

-- TODO: Improve this...
render :: DocPage -> Text
render (Man m) = _manPageRest m
render (LongHelp (HelpPage _ _ x)) = x
render (ShortHelp (HelpPage _ _ x)) = x
