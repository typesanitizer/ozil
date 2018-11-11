module Help.Page.Help where

import Commons

data HelpPage = HelpPage
  { _helpPageHeading  :: Optional
  , _helpPageSynopsis :: Optional -- ^ Equivalent to "usage"
  , _helpPageBody     :: Vector Item
  }

-- TODO: Maybe we should record offsets here?
data Item
  = Subcommand { _name :: Text, _description :: Text }
  | Flags      { _name :: Text, _description :: Text }
  | Plain Text
