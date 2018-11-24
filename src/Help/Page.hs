{-# LANGUAGE ViewPatterns #-}

module Help.Page
  ( DocPage (..)
  , parseHelpPage
  , parseManPage
  , DocPageSummary (..)
  , ManPageSummary
  , parseManPageSummary
  , HelpPageSummary (..)
  , displayHelpPageSummary
  , BinaryPath (Global, Local)

  , PagePath
  , summaryToPath
  , getSummary

  , LinkState
  , mkLinkStateOff
  , flipLinkState
  , mapLinkState
  , isOn

  , highlightedSubcommand
  , getNewSubcommand

  , getDocPage
  , getHelpPageSummary

  , render
  )
  where

import Commons

import Development.BuildSystem (getHelpTextViaBS, mkExecArgs)
import Help.Page.Help
import Help.Page.Internal
import Help.Page.Lenses (binaryPath, section, name, subcommandPath, anchors)

import Help.Page.Man
  (ManPageView (..), ManPage (..), parseWhatisDescription, parseManPage)
import Help.Subcommand (mkSubcommand, Subcommand)

import Brick hiding (txt, render)

import Brick.FastMarkup (fmWrap)
import Codec.Compression.GZip (decompress)
import Data.List.Extra (trim)
import Data.Monoid (Sum(..))
import System.FilePath
  ((</>), splitDrive, splitDirectories, splitFileName, takeExtension)
import System.Process (readProcess)
import Text.Wrap (WrapSettings (..))

import qualified Brick
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Generic as V
import qualified Graphics.Vty as Vty

--------------------------------------------------------------------------------
-- * Working with summaries

----------------------------------------------------------------------
-- ** Parsing

parseManPageSummary :: String -> Either (ParseFail String) ManPageSummary
parseManPageSummary = parseWhatisDescription

----------------------------------------------------------------------
-- ** User facing display

displayHelpPageSummary :: HelpPageSummary -> String
displayHelpPageSummary (HelpPageSummary bp scp sh _) =
  case bp of
    Global p -> unwords (abbrev p : map show scp <> [hstr])
      where abbrev (splitFileName -> (ds, fn)) =
              let (dr, splitDirectories -> dirs) = splitDrive ds
                  middir = maybe ".." (</> "..") (headMaybe dirs)
              in dr </> middir </> fn
    Local{} -> (\(exe, rest) -> unwords (exe : rest <> [hstr]))
      $ mkProcessArgs bp scp
  where
    hstr = if sh then "-h" else "--help"

mkProcessArgs :: BinaryPath -> [Subcommand] -> (String, [String])
mkProcessArgs bp subcs = case bp of
  Global fp -> (fp, map show subcs)
  Local _pf bs bin -> mkExecArgs bs bin subcs

----------------------------------------------------------------------
-- ** Fetching documentation using summaries

getDocPage :: DocPageSummary -> IO (Maybe DocPage)
getDocPage = \case
  ManSummary  m -> getManPage m
  HelpSummary h -> getHelpPage h

-- TODO: Try to catch more errors here.
getManPage :: ManPageSummary -> IO (Maybe DocPage)
getManPage mps = do
  let (n, s) = (mps ^. name, mps ^. section)
  path <- trim <$> readProcess "man" ["-S", s, "-w", n] ""
  -- TODO: Man pages might be in some other encoding like Latin1?
  -- TODO: Man pages might be stored in other formats?
  txt <- if takeExtension path == ".gz"
    then T.decodeUtf8 . BS.toStrict . decompress <$> BS.readFile path
    else T.readFile path
  pure (Just (Man mps (parseManPage txt)))

getHelpPageSummary :: BinaryPath -> [Subcommand] -> IO (Maybe HelpPageSummary)
getHelpPageSummary binPath subcPath = do
  d1 <- go ["-h"]
  d2 <- fmap (mkHPS False) <$> go ["--help"]
  pure $ maybe d2 (Just . mkHPS True) d1
  where
    mkHPS = HelpPageSummary binPath subcPath
    go hstr = case binPath of
      Global fp -> readProcessSimple fp (map show subcPath <> hstr)
      Local _pf bs bin -> getHelpTextViaBS bs bin subcPath hstr

getManPageSummary :: Text -> Text -> IO (Maybe ManPageSummary)
getManPageSummary (unpack -> name_) (unpack -> section_) = do
  out <- readProcessSimple "whatis" [name_, "-s", section_]
  let eitherToMaybe = \case Left _ -> Nothing; Right x -> Just x;
  pure $ (eitherToMaybe . parseManPageSummary
          <=< headMaybe . lines . unpack) =<< out

getHelpPage :: HasCallStack => HelpPageSummary -> IO (Maybe DocPage)
getHelpPage hsum@(HelpPageSummary binPath subcPath short _) =
  let hstr = if short then ["-h"] else ["--help"]
  in fmap (Help hsum . parseHelpPage) <$> go hstr
  where
    go hstr = case binPath of
      Global fp -> readProcessSimple fp (map show subcPath <> hstr)
      Local _pf bs bin -> getHelpTextViaBS bs bin subcPath hstr

----------------------------------------------------------------------
-- ** Saving summaries for later use

summaryToPath :: DocPageSummary -> PagePath
summaryToPath = \case
  HelpSummary h -> HelpPath (h ^. binaryPath)
  ManSummary w ->
    ManPath { _fullName = pack (w ^. name), _section = pack (w ^. section) }

getSummary :: [Subcommand] -> PagePath -> IO (Maybe DocPageSummary)
getSummary subcs = \case
  ManPath{_fullName, _section} ->
    fmap ManSummary <$> getManPageSummary _fullName _section
  HelpPath h -> fmap HelpSummary <$> getHelpPageSummary h subcs

--------------------------------------------------------------------------------
-- * Working with LinkState

mkLinkStateOff :: DocPage -> LinkState
mkLinkStateOff = \case
  Man{} -> LinksOff 0 0 -- FIXME: Unimplemented logic.
  Help _ hp -> LinksOff (V.length (hp ^. anchors)) 0

flipLinkState :: LinkState -> LinkState
flipLinkState = \case
  LinksOff c x -> LinksOn c x
  LinksOn c x -> LinksOff c x

mapLinkState :: (Int -> Int) -> LinkState -> LinkState
mapLinkState f = \case
  LinksOff c x -> LinksOff c x
  LinksOn c i -> LinksOn c (inBounds 0 (f i) (c - 1))

isOn :: LinkState -> Bool
isOn LinksOff{} = False
isOn LinksOn{}  = True

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
    let hsum' = over subcommandPath (++ [subc]) hsum
    getHelpPage hsum'

--------------------------------------------------------------------------------
-- * Rendering pages

render :: LinkState -> DocPage -> Widget n
render ls = \case
  Man _ m -> renderManPage m
  Help _ h -> renderHelpPage ls h

renderManPage :: ManPage -> Widget n
renderManPage (ManPage (ManPageView _pre h _post sections fm) _) =
  Brick.str (show $ getSum (foldMap (Sum . V.length . snd) sections))
  ===
  vBox (renderHeading h : each renderSection sections)
  where
    renderHeading = const emptyWidget
    each f = V.toList . V.imap f
    renderSection i (sh, _chnks) = vBox
      [renderSectionHeading sh, fmWrap (fm !!! i)]
    renderSectionHeading =
      Brick.modifyDefAttr (`Vty.withStyle` Vty.bold) . Brick.txt

renderHelpPage :: LinkState -> HelpPage -> Widget n
renderHelpPage ls HelpPage{_helpPageBody = v, _helpPageAnchors = a} =
  vBox . zipWith renderItem [0 ..] $ V.toList v
  where
    renderItem i = \case
      Plain t -> txtWrap t
      Tabular tt ents inds -> vBox . V.toList
        $ V.imap (\j -> renderEntry i j tt inds) ents

    defaultRightPadding = 4
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
        itemFits :: a -> a -> a
        itemFits tb fb = if gap > 0 then tb else fb
        itemWidth = textWidth item
        highlight = case ls of
          LinksOff{} -> id
          LinksOn{} | tblTy /= Subcommand  -> id
          LinksOn _ k -> withAttr
            $ if a !!! k == (i, j) then "subc-highlight" else "subc-link"
        delta_x = itemFits gap 4
        extraIndent = hLimit delta_x (vLimit 1 (fill ' '))
