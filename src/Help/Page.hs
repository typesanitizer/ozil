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
  ( parseManPage, emptyManPage, ManPage (..), ManPageView (..)
  , WhatisDescription (..), parseWhatisDescription, Chunk (..)
  )
import Help.Page.Help

import Brick hiding (txt, render)
import Brick.FastMarkup (mkFastMarkup, fmWrap)

import Brick.Markup (markup, Markup)
import Data.Monoid (Sum(..))
import Data.Text.Markup (fromText)
import Text.Wrap (WrapSettings (..))

import qualified Brick
import qualified Data.Vector.Generic as V

--------------------------------------------------------------------------------
-- * Pages

data DocPage
  = Man       { _docPageManPage  :: ManPage  }
  | LongHelp  { _docPageHelpPage :: HelpPage }
  | ShortHelp { _docPageHelpPage :: HelpPage }

data ManPageSummary
  = WhatisDescr !WhatisDescription
  | UnknownFormat { _shortDescription :: String }

parseManPageSummary :: String -> ManPageSummary
parseManPageSummary s =
  maybe (UnknownFormat s) WhatisDescr (parseWhatisDescription s)

-- FIXME: Erm, these record field names don't make sense. Available and Text?
data HelpPageSummary = HelpPageSummary
  { binaryPath         :: FilePath
  , shortHelpAvailable :: !Bool
  , shortHelpText      :: Text
  }

data DocPageSummary
  = ManSummary  !ManPageSummary
  | HelpSummary !HelpPageSummary

parseLongHelp :: Text -> HelpPage
parseLongHelp = parseHelpPage

-- TODO: Improve this...
parseShortHelp :: Text -> HelpPage
parseShortHelp = parseLongHelp

parseMan :: Text -> ManPage
parseMan = parseManPage

data LinkState
  = LinksOff {len :: !Int, _prev :: !Int}
  | LinksOn  {len :: !Int, _sel  :: !Int}
  deriving Show

mkLinkStateOff :: DocPage -> LinkState
mkLinkStateOff = \case
  Man{} -> LinksOff 0 0
  LongHelp  HelpPage{_helpPageAnchors = t} -> LinksOff (V.length t) 0
  ShortHelp HelpPage{_helpPageAnchors = t} -> LinksOff (V.length t) 0

flipLinkState :: LinkState -> LinkState
flipLinkState = \case
  LinksOff c x -> LinksOn c x
  LinksOn c x -> LinksOff c x

mapLinkState :: (Int -> Int) -> LinkState -> LinkState
mapLinkState f = \case
  LinksOff c x -> LinksOff c x
  LinksOn c i -> LinksOn c (inBounds 0 (f i) (c - 1))

render :: LinkState -> DocPage -> Widget n
render ls = \case
  Man m -> renderManPage m
  LongHelp h -> renderHelpPage ls h
  ShortHelp h -> renderHelpPage ls h

renderManPage :: ManPage -> Widget n
renderManPage (ManPage (ManPageView h secs _) _) =
  Brick.str (show $ getSum (foldMap (Sum . V.length . snd) secs))
  ===
  vBox (renderHeading h : each renderSection secs)
  where
    renderHeading = const emptyWidget
    each f = V.toList . V.map f
    renderSection (sh, chnks) = vBox
      [renderSectionHeading sh, fmWrap $ mkFastMarkup
        $ V.toList $ fmap ((,"subc-link" :: AttrName) . renderChunk) chnks]
    renderSectionHeading = Brick.txt
    renderChunk (Chunk txt _) = txt

renderHelpPage :: LinkState -> HelpPage -> Widget n
renderHelpPage ls HelpPage{_helpPageBody = v, _helpPageAnchors = a} =
  vBox . zipWith renderItem [0 ..] $ V.toList v
  where
    renderItem i = \case
      Plain t -> txtWrap t
      Tabular tt ents inds -> vBox . V.toList
        $ V.imap (\j -> renderEntry i j tt inds) ents

    defaultRightPadding = 4
    -- TODO: There is still an off-by-one error hiding somewhere. Try ozil ozil.
    -- We _could_ "fix" it by adding a (- 1) to the gap but is that right?
    renderEntry i j tblTy
      ItemIndent{itemIndent, descIndent}
      TableEntry{_name=item, _description=desc}
      = padRight (Pad defaultRightPadding)
      $ padLeft (Pad itemIndent) (layout [itemWidget, descWidget])
      where
        ws = WrapSettings{preserveIndentation=False, breakLongWords=True}
        layout = itemFits hBox (padTopBottom 1 . vBox)
        itemWidget = highlight (txtWrapWith ws item)
        descWidget = hBox [extraIndent, hLimit (itemFits 55 66) $ txtWrap desc]
        gap = descIndent - itemWidth - itemIndent
        itemFits tb fb = if gap > 0 then tb else fb
        itemWidth = textWidth item
        highlight = case ls of
          LinksOff{} -> id
          LinksOn{} | tblTy /= Subcommand  -> id
          LinksOn _ k -> withAttr
            $ if a V.! k == (i, j) then "subc-highlight" else "subc-link"
        delta_x = itemFits gap 4
        extraIndent = hLimit delta_x (vLimit 1 (fill ' '))
