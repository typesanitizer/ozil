module Help.Page.Man where

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
  { _manPageHeading :: !Heading
  , _manPageName :: !(Maybe Text)
  , _manPageSynopsis :: !(Maybe Text)
  , _manPageConfiguration :: Maybe Text
  , _manPageDescription :: !(Maybe Text)
  , _manPageOptions :: Maybe Text
  , _manPageExitStatus :: Maybe Text
  , _manPageReturnValue :: Maybe Text
  , _manPageErrors :: Maybe Text
  , _manPageEnvironment :: Maybe Text
  , _manPageFiles :: Maybe Text
  , _manPageVersions :: Maybe Text
  , _manPageAttributes :: Maybe Text
  , _manPageConformingTo :: Maybe Text
  , _manPageNotes :: Maybe Text
  , _manPageBugs :: Maybe Text
  , _manPageExample :: Maybe Text
  , _manPageSeeAlso :: Maybe Text
  , _manPageRest :: Text
  }
