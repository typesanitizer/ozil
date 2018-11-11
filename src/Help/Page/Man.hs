module Help.Page.Man where

import Commons

import Help.Page.Help (Optional)
import Text.Megaparsec (Parsec, some, (<|>), between, parseMaybe)
import Text.Megaparsec.Char (anyChar, space1, char, alphaNumChar)

data Heading = ManHeading
  { _headingTitle   :: !Text
  , _headingDate    :: !Text
  , _headingSection :: !Text
  , _headingSource  :: !Text
  , _headingManual  :: !Text
  }

emptyHeading :: Heading
emptyHeading = ManHeading "" "" "" "" ""

-- | Structure of a typical man-page
-- See man-pages(7).
data ManPage = ManPage
  { _manPageHeading       :: !Heading
  , _manPageName          :: !Optional
  , _manPageSynopsis      :: !Optional
  , _manPageConfiguration :: !Optional
  , _manPageDescription   :: !Optional
  , _manPageOptions       :: !Optional
  , _manPageExitStatus    :: !Optional
  , _manPageReturnValue   :: !Optional
  , _manPageErrors        :: !Optional
  , _manPageEnvironment   :: !Optional
  , _manPageFiles         :: !Optional
  , _manPageVersions      :: !Optional
  , _manPageAttributes    :: !Optional
  , _manPageConformingTo  :: !Optional
  , _manPageNotes         :: !Optional
  , _manPageBugs          :: !Optional
  , _manPageExample       :: !Optional
  , _manPageSeeAlso       :: !Optional
  , _manPageRest          :: !Text
  }

emptyManPage :: ManPage
emptyManPage = ManPage emptyHeading
    mempty mempty mempty mempty mempty mempty mempty
    mempty mempty mempty mempty mempty mempty
    mempty mempty mempty mempty mempty

-- Okay to use strings because these are going to be quite short.
data WhatisDescription = WhatisDescription
  { _whatisDescriptionName             :: String
  , _whatisDescriptionSection          :: String
  , _whatisDescriptionShortDescription :: String
  }

parseWhatisDescription :: String -> Maybe WhatisDescription
parseWhatisDescription = parseMaybe p
  where
    p :: Parsec Void String WhatisDescription
    p = do
      name <- some (alphaNumChar <|> char '-')
      space1
      sec <- between (char '(') (char ')') (some (alphaNumChar <|> char '-'))
      space1 *> char '-' *> space1
      descr <- some anyChar
      pure (WhatisDescription name sec descr)
