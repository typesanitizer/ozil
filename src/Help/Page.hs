module Help.Page where

import Commons

import Help.Page.Man
  ( emptyManPage, ManPage (..), WhatisDescription (..), parseWhatisDescription )
import Help.Page.Help

import Brick hiding (txt)
import Brick.Widgets.Border (border)

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

render :: DocPage -> Widget n
render = \case
  Man m -> txtWrap (_manPageRest m)
  LongHelp (HelpPage _ _ x _) -> ws x
  ShortHelp (HelpPage _ _ x _) -> ws x
  where
    ws v = vBox $ map renderItem (V.toList v)
    renderItem = \case
      Plain t -> txtWrap t
      Tabular _ ents inds -> vBox . V.toList $ V.map (renderEntry inds) ents
    defaultPadding = 4
    renderEntry (ItemIndent ii di) (TableEntry itm descr) =
      let n = textWidth itm
          iw = txtWrap itm
          itemFits = ii + n < di
          delta_x = if itemFits then di - ii - n else 4
          extraIndent = hLimit delta_x (vLimit 1 (fill ' '))
          dw = hBox [extraIndent, hLimit (if itemFits then 55 else 66) $ txtWrap descr]
          lyt = if itemFits then hBox else padTopBottom 1 . vBox
      in padLeftRight defaultPadding (lyt [iw, dw])
