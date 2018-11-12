module Help.Page where

import Commons

import Help.Page.Man
  ( emptyManPage, ManPage (..), WhatisDescription (..), parseWhatisDescription )
import Help.Page.Help (HelpPage (..), Item (Plain), parsePickAnchors)

import Brick (Widget)

import qualified Brick
import qualified Data.Text as T
import qualified Data.Vector.Generic as V

--------------------------------------------------------------------------------
-- * Pages

data DocPage
  = Man { _manPage :: ManPage }
  | LongHelp { _longHelpPage :: HelpPage }
  | ShortHelp { _shortHelpPage :: HelpPage }

data ManPageSummary
  = WhatisDescr !WhatisDescription
  | UnknownFormat { _shortDescription :: String }

parseManPageSummary :: String -> ManPageSummary
parseManPageSummary s =
  maybe (UnknownFormat s) WhatisDescr (parseWhatisDescription s)

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
  { _helpPageHeading  = Nothing
  , _helpPageSynopsis = Nothing
  , _helpPageBody     = items
  , _helpPageAnchors  = anchors
  }
  where (items, anchors) = parsePickAnchors txt

-- TODO: Improve this...
parseShortHelp :: Text -> HelpPage
parseShortHelp = parseLongHelp

parseMan :: Text -> ManPage
parseMan txt = emptyManPage { _manPageRest = txt }

-- TODO: Improve this...
render :: DocPage -> Widget n
render = \case
  Man m -> Brick.txtWrap (_manPageRest m)
  LongHelp (HelpPage _ _ x _) -> Brick.txtWrap (getPlain x)
  ShortHelp (HelpPage _ _ x _) -> Brick.txtWrap (getPlain x)
  where
    getPlain v = T.concat [t | Plain t <- V.toList v]
