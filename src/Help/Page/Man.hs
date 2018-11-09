module Help.Page.Man where

import Help.Page.Help (Optional)
import Data.Text (Text)

data Heading = ManHeading
  { _headingTitle :: !Text
  , _headingDate :: !Text
  , _headingSection :: !Text
  , _headingSource :: !Text
  , _headingManual :: !Text
  }

-- | Structure of a typical man-page
-- See man-pages(7).
data ManPage = ManPage
  { _manPageHeading :: Heading
  , _manPageName :: Optional
  , _manPageSynopsis :: Optional
  , _manPageConfiguration :: Optional
  , _manPageDescription :: Optional
  , _manPageOptions :: Optional
  , _manPageExitStatus :: Optional
  , _manPageReturnValue :: Optional
  , _manPageErrors :: Optional
  , _manPageEnvironment :: Optional
  , _manPageFiles :: Optional
  , _manPageVersions :: Optional
  , _manPageAttributes :: Optional
  , _manPageConformingTo :: Optional
  , _manPageNotes :: Optional
  , _manPageBugs :: Optional
  , _manPageExample :: Optional
  , _manPageSeeAlso :: Optional
  , _manPageRest :: Text
  }
