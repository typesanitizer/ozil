module Help.Page.Help where

import Data.Text (Text)

data HelpPage = HelpPage
  { _helpPageHeading :: Maybe Text
  , _helpPageSynopsis :: Maybe Text   -- ^ Equivalent to "usage"
  , _helpPageRest :: Text
  }
