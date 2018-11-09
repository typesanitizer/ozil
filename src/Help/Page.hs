module Help.Page where

import Help.Page.Man
import Help.Page.Help

data DocPage
  = Man { _manPage :: ManPage }
  | LongHelp { _longHelpPage :: HelpPage }
  | ShortHelp { _shortHelpPage :: HelpPage }

newtype ManPageInfo = ManPageInfo { _shortDescription :: String }

newtype HelpPageInfo = HelpPageInfo { _binaryPath :: FilePath }
