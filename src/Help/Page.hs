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

newtype ManPageInfo = ManPageInfo { _shortDescription :: String }

newtype HelpPageInfo = HelpPageInfo { _binaryPath :: FilePath }

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
parseMan txt = emptyManPage & set rest txt

-- TODO: Improve this...
render :: DocPage -> Text
render (Man m) = _manPageRest m
render (LongHelp (HelpPage _ _ x)) = x
render (ShortHelp (HelpPage _ _ x)) = x
