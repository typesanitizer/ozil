module Help.Page
  ( DocPage (..)
  , parseHelpPage
  , parseManPage
  , DocPageSummary (..)
  , ManPageSummary (..)
  , parseManPageSummary
  , HelpPageSummary (..)

  , LinkState
  , mkLinkStateOff
  , flipLinkState
  , mapLinkState

  , highlightedSubcommand
  , getNewSubcommand

  , getDocPage
  , getLongHelp
  , getShortHelp

  , render
  )
  where

import Commons

import Help.Page.Help
import Help.Page.Internal
import Help.Page.Lenses (section, name, subcommandPath, anchors)

import Help.Page.Man
  (ManPageView (..), ManPage (..), parseWhatisDescription, parseManPage)
import Help.Subcommand (mkSubcommand, Subcommand)

import Brick hiding (txt, render)

import Brick.FastMarkup (fmWrap)
import Codec.Compression.GZip (decompress)
import Data.List.Extra (trim)
import Data.Monoid (Sum(..))
import System.FilePath (takeExtension)
import System.Process (readProcess)
import Text.Wrap (WrapSettings (..))

import qualified Brick
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Generic as V

--------------------------------------------------------------------------------
-- * Parsing

parseManPageSummary :: String -> ManPageSummary
parseManPageSummary s =
  maybe (UnknownFormat s) WhatisDescr (parseWhatisDescription s)

--------------------------------------------------------------------------------
-- * Working with LinkState

mkLinkStateOff :: DocPage -> LinkState
mkLinkStateOff = \case
  Man{} -> LinksOff 0 0 -- FIXME: Unimpelemented logic.
  Help _ hp -> LinksOff (V.length (hp ^. anchors)) 0

flipLinkState :: LinkState -> LinkState
flipLinkState = \case
  LinksOff c x -> LinksOn c x
  LinksOn c x -> LinksOff c x

mapLinkState :: (Int -> Int) -> LinkState -> LinkState
mapLinkState f = \case
  LinksOff c x -> LinksOff c x
  LinksOn c i -> LinksOn c (inBounds 0 (f i) (c - 1))

--------------------------------------------------------------------------------
-- ** Working with subcommands

highlightedSubcommand :: LinkState -> DocPage -> Maybe Subcommand
highlightedSubcommand = f
  where
    f LinksOff{} _ = Nothing
    f _ Man{}      = Nothing -- FIXME: Implement this
    f LinksOn{len, _sel} (Help _ h) = if len == 0 then Nothing else
      let err = error "Error: Improper index stored."
          TableEntry{_name} = fromMaybe err (getEntry _sel h)
      in Just (mkSubcommand (unpack _name))

getNewSubcommand :: Subcommand -> DocPage -> IO (Maybe DocPage)
getNewSubcommand subc dp = case dp of
  Man{} -> undefined
  Help hsum _ -> do
    let hsum' = over subcommandPath (subc:) hsum
    getHelpPage hsum'

--------------------------------------------------------------------------------
-- * Fetching documentation

getDocPage :: DocPageSummary -> IO (Maybe DocPage)
getDocPage = \case
  ManSummary  m -> getManPage m
  HelpSummary h -> getHelpPage h

-- TODO: Try to catch more errors here.
getManPage :: HasCallStack => ManPageSummary -> IO (Maybe DocPage)
getManPage msum@(WhatisDescr w) = do
  let (n, s) = (w ^. name, w ^. section)
  path <- trim <$> readProcess "man" ["-S", s, "-w", n] ""
  -- TODO: Man pages might be in some other encoding like Latin1?
  -- TODO: Man pages might be stored in other formats?
  txt <- if takeExtension path == ".gz"
    then T.decodeUtf8 . BS.toStrict . decompress <$> BS.readFile path
    else T.readFile path
  pure (Just (Man msum (parseManPage txt)))
getManPage (UnknownFormat _) =
  -- TODO: Error handling
  error "Error: Unexpected format for man page summary."

getShortHelp :: FilePath -> [Subcommand] -> IO (Maybe Text)
getShortHelp binPath subcPath
  = readProcessSimple binPath (map show subcPath <> ["-h"])

getLongHelp :: FilePath -> [Subcommand] -> IO (Maybe Text)
getLongHelp binPath subcPath
  = readProcessSimple binPath (map show subcPath <> ["--help"])

getHelpPage :: HasCallStack => HelpPageSummary -> IO (Maybe DocPage)
getHelpPage hsum@(HelpPageSummary binPath subcPath short _) =
  let get = if short then getShortHelp else getLongHelp
  in fmap (Help hsum . parseHelpPage) <$> get binPath subcPath

--------------------------------------------------------------------------------
-- * Rendering

render :: LinkState -> DocPage -> Widget n
render ls = \case
  Man _ m -> renderManPage m
  Help _ h -> renderHelpPage ls h

renderManPage :: ManPage -> Widget n
renderManPage (ManPage (ManPageView h sections _ fm) _) =
  Brick.str (show $ getSum (foldMap (Sum . V.length . snd) sections))
  ===
  vBox (renderHeading h : each renderSection sections)
  where
    renderHeading = const emptyWidget
    each f = V.toList . V.imap f
    -- TODO: unused index. This can be fixed once we get rid of the
    -- Twinkle twinkle little star.
    renderSection _i (sh, _chnks) = vBox
      [renderSectionHeading sh, fmWrap (fm !!! 0)]
    renderSectionHeading = Brick.txt

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
            $ if a !!! k == (i, j) then "subc-highlight" else "subc-link"
        delta_x = itemFits gap 4
        extraIndent = hLimit delta_x (vLimit 1 (fill ' '))
