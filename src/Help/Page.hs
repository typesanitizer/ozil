module Help.Page
  ( DocPage (..)
  , ManPageSummary (..)
  , parseManPageSummary
  , HelpPageSummary (..)

  , parseShortHelp
  , parseLongHelp
  , parseMan

  , DocPageSummary (..)

  , LinkState
  , mkLinkStateOff
  , flipLinkState
  , mapLinkState

  , render
  )
  where

import Commons

import Help.Page.Man
  ( emptyManPage, ManPage (..), WhatisDescription (..), parseWhatisDescription )
import Help.Page.Help

import Brick hiding (txt, render)

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
  , _helpPageAnchors  = anchorIxs
  , _helpPageTableIxs = tableIxs
  }
  where (items, tableIxs, anchorIxs) = parsePickAnchors txt

-- TODO: Improve this...
parseShortHelp :: Text -> HelpPage
parseShortHelp = parseLongHelp

parseMan :: Text -> ManPage
parseMan txt = emptyManPage { _manPageRest = txt }

data LinkState
  = LinksOff {len :: !Int, _prev :: !Int}
  | LinksOn  {len :: !Int, _sel  :: !Int}
  deriving Show

mkLinkStateOff :: DocPage -> LinkState
mkLinkStateOff = \case
  Man{} -> LinksOff 0 0
  LongHelp  HelpPage{_helpPageTableIxs = t} -> LinksOff (V.length t) 0
  ShortHelp HelpPage{_helpPageTableIxs = t} -> LinksOff (V.length t) 0

flipLinkState :: LinkState -> LinkState
flipLinkState = \case
  LinksOff c x -> LinksOn c x
  LinksOn c x -> LinksOff c x

mapLinkState :: (Int -> Int) -> LinkState -> LinkState
mapLinkState f = \case
  LinksOff c x -> LinksOff c x
  LinksOn c i -> LinksOn c (inBounds 0 (f i) (c - 1))

render :: LinkState -> DocPage -> Widget n
render _ = \case
  Man m -> txtWrap (_manPageRest m)
  LongHelp  HelpPage{_helpPageBody = x} -> ws x
  ShortHelp HelpPage{_helpPageBody = x} -> ws x
  where
    ws v = vBox $ map renderItem (V.toList v)
    renderItem = \case
      Plain t -> txtWrap t
      Tabular _ ents inds -> vBox . V.toList $ V.map (renderEntry inds) ents
    defaultPadding = 4
    renderEntry (ItemIndent ii di) (TableEntry itm descr) =
      -- TODO: improve line-breaking for flags because some programs have really
      -- long options. Here's one from rustc:
      -- --print [crate-name|file-names|sysroot|cfg|target-list|target-cpus|target-features|relocation-models|code-models|tls-models|target-spec-json|native-static-libs]
      -- WTF mate.
      -- The widget available in Brick doesn't understand that one can break in
      -- between at the |'s.
      let n = textWidth itm
          iw = txtWrap itm
          itemFits = ii + n < di
          delta_x = if itemFits then di - ii - n else 4
          extraIndent = hLimit delta_x (vLimit 1 (fill ' '))
          dw = hBox [extraIndent, hLimit (if itemFits then 55 else 66) $ txtWrap descr]
          lyt = if itemFits then hBox else padTopBottom 1 . vBox
      in padLeftRight defaultPadding (lyt [iw, dw])
