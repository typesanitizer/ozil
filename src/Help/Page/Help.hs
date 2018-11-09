module Help.Page.Help where

import Data.Text (Text)

type Optional = Maybe Text

data HelpPage = HelpPage
  { _helpPageHeading :: Optional
  , _helpPageSynopsis :: Optional -- ^ Equivalent to "usage"
  , _helpPageRest :: Text
  }
