module Help.Page.Man.Internal
  ( WhatisDescription (..)
  , parseWhatisDescription
  , Heading (..)
  , ManPage (..)
  , emptyManPage
  , ManPageView (..)
  , ManPageMetadata (..)
  , Chunk (Chunk)
  , parseManPage
  , manHeadingP

  -- * Exports for testing only
  , manPageLineP
  , ManPageLine (..)
  , Chunks
  , directiveLineP
  , runManParser
  , PE
  ) where

import Commons

import Brick (AttrName)
import Brick.FastMarkup (FastMarkup, mkFastMarkup)
import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char as C

import Data.Char (isSpace, isAlphaNum)
import Data.HashSet (HashSet)
import Data.List.Split (chop)
import Lens.Micro ((%~))

import qualified Data.Text as T
import qualified Data.HashSet as Set
import qualified Data.Vector.Generic as V

--------------------------------------------------------------------------------
-- * Whatis descriptions

-- Okay to use strings because these are going to be quite short.
data WhatisDescription = WhatisDescription
  { _whatisDescriptionName             :: String
  , _whatisDescriptionSection          :: String
  , _whatisDescriptionShortDescription :: String
  } deriving (Eq, Show)

parseWhatisDescription :: String -> Maybe WhatisDescription
parseWhatisDescription = parseMaybe p
  where
    p :: Parsec Void String WhatisDescription
    p = do
      name <- lexeme binNameP
      sec <- between (char '(') (char ')') secNumP
      space1 *> char '-' *> space1
      desc <- some anyChar
      pure (WhatisDescription name sec desc)

--------------------------------------------------------------------------------
-- * Man page heading

data Heading = ManHeading
  { _headingTitle   :: !Text
  , _headingSection :: !Text
  , _headingDate    :: !Text
  , _headingSource  :: !Text
  , _headingManual  :: !Text
  } deriving (Eq, Show)

emptyHeading :: Heading
emptyHeading = ManHeading "" "" "" "" ""

manHeadingP :: MonadParsec e Text m => m Heading
manHeadingP = do
  -- Why the hell does Data.List not have a safe indexing operator.
  chunks <- V.fromList <$> some ((quoted <|> takeWhile1P' (not . isSpace)) <* space)
  void (space *> optional eof)
  let at i = fromMaybe "" ((chunks :: Vector Text) V.!? i)
      _headingTitle = at 0
      _headingSection = at 1
      _headingDate = at 2
      _headingSource = at 3
      _headingManual = at 4
  pure $ ManHeading
    { _headingTitle
    , _headingSection
    , _headingDate
    , _headingSource
    , _headingManual
    }
  where quoted = between (char '"') (char '"') (takeWhile1P' (/= '"'))

--------------------------------------------------------------------------------
-- * Man page

-- | Structure of a typical man-page.
-- See man-pages(7).
data ManPage = ManPage
  { _manPageView     :: !ManPageView
  , _manPageMetadata :: !ManPageMetadata
  }

data ManPageView = ManPageView
  { _manPagePreHeading  :: !Chunks
  , _manPageViewHeading :: !Heading
  , _manPagePostHeading :: !Chunks
  , _manPageViewSection :: !(Vector (Text, Chunks))
  , _manPageFastMarkup  :: !(Vector (FastMarkup Brick.AttrName))
  }

data ManPageMetadata = ManPageMetadata
  { _manPageMetadataOriginal     :: !(Vector Text)
  , _manPageMetadataUnrecognized :: !(UVector Int)
  }

emptyManPage :: ManPage
emptyManPage = ManPage
  (ManPageView mempty emptyHeading mempty mempty mempty)
  (ManPageMetadata mempty mempty)

-- Ossanna and Kernighan's "Troff User's manual" seems to at least have the
-- comment syntax "correct" (it matches up with man.1). I guess I will just have
-- to fumble my way through this.
--
-- Any new insights should probably be documented on Unix.SE:
-- https://unix.stackexchange.com/q/481025/89474
--
-- Also, it looks like man(1) and git(1) use totally different dialects of roff?
-- But the share the same 'tbl' prepocessor? ...
parseManPage :: Text -> ManPage
parseManPage t =
  ManPage
  (ManPageView preHeading theHeading postHeading sections fm)
  (ManPageMetadata (V.fromList inputLines) (V.reverse $ V.fromList finalPS))
  where
    inputLines = T.lines t

    (fullParseResult, PS finalPS) =
      zip [1 ..] inputLines
      & traverse (\(i, line) -> runParserT (manPageLineP i) "" line)
      & flip runState (PS [])
      & _1 %~ sequenceA

    parsedLines = filter (not . isComment)
      $ either (error . show) id fullParseResult

    coerceMarkup = \case
      Markup c -> c
      z -> error ("Expected markup but found " <> show z)

    (preHeading, theHeading, postHeading, sections) =
      let (pre, Heading h : rest) = case break isHeading parsedLines of
            (x, y@(Heading _ : _)) -> (x, y)
            (x, y) -> error ("Expected heading. Got something else :(\n"
                             <> show (take 20 x) <> "\n" <> show (take 10 y))
          (post, secs) = break isSectionHeading $ map coerceMarkup rest
      in (makePreHeading pre, h, makePostHeading post, mkSections secs)

    makePreHeading = coalesceChunks . map coerceMarkup
    makePostHeading = coalesceChunks
    isHeading = \case Heading _ -> True;  Comment _ -> False; Markup _ -> False
    isComment = \case Heading _ -> False; Comment _ -> True;  Markup _ -> False
    isSectionHeading (Chunk d _) = d == Dir "SH"

    fm = V.map (mkFastMarkup . V.toList . V.concatMap chunkToFM . snd) sections

    -- Git's man page puts quotes around section headings.
    dropQuotes z = case T.uncons z of
      Just ('"', rest) -> T.init rest
      _ -> z

    mkSections :: [Chunk] -> Vector (Text, Chunks)
    mkSections = V.fromList . catMaybes . chop
        (\(ch:chs) -> case ch of
            Chunk (Dir "SH") sh ->
              let (chks, rest) = break isSectionHeading chs
              in (Just (dropQuotes sh, coalesceChunks chks), rest)
            Chunk _ _ ->
              error (printf "Found stray chunk %s even though it was supposed to be\
                            \ gobbled by a section heading." (show ch))
        )

chunkToFM :: V.Vector v (Text, AttrName) => Chunk -> v (Text, AttrName)
chunkToFM (Chunk d z) =
  let z' = rewrite z
      alt t a1 a2 = V.fromList
        [ (x, a) | (i, x) <- zip [0 ..] (T.splitOn "\\|" t)
                 , let a = if i `mod` 2 == 0 then a1 else a2
        ]
      res  = if
        | d == Dir "B"  -> solo z' "man-B"
        | d == Dir "I"  -> solo z' "man-I"
        | d == Dir "RB" -> alt  z' "man-default" "man-B"
        | d == Dir "BR" -> alt  z' "man-B" "man-default"
        | d == Dir "RI" -> alt  z' "man-default" "man-I"
        | d == Dir "IR" -> alt  z' "man-I" "man-default"
        | d == Dir "br" -> solo "\n" "man-default"
        | z' == "\\&.\\|.\\|.\\&" -> solo "..." "man-default"
        | otherwise     -> solo z' "man-default"
  in res
  where
    solo = curry V.singleton
    rewrite = T.replace "\\-" "-"

coalesceChunks :: [Chunk] -> Vector Chunk
coalesceChunks = V.fromList . chop groupConcat
  where
    groupConcat [] = error "unreachable in chop"
    groupConcat (Chunk d txt : chks) =
        let (txts, rest) = flip mapSpan chks $ \(Chunk d' z) ->
              if d' == d then Just z
              else Nothing
        in (Chunk d (T.intercalate " " (txt : txts)), rest)
    mapSpan f xs =
      let (pre, post) = span (isJust . fst) (zip (map f xs) xs)
      in (map (fromMaybe (error "unreachable") . fst) pre, map snd post)

--------------------------------------------------------------------------------
-- * Helper functions

type Parser = ParsecT () Text (State PS)

type PE = ParseError Char ()

runManParser :: Parser a -> Text -> (Either PE a, PS)
runManParser p t = runState (runParserT p "" t) (PS [])

data Directive = None | Dir !Text
  deriving (Eq, Show)

-- See man(7) (e.g. @man -S 7 man@) for details.
knownDirectives :: HashSet Text
knownDirectives = Set.fromList
  [ "SH" -- Section heading
  , "B" , "BR", "RB" -- Bold (and alternating with roman)
  , "I", "IR", "RI"  -- Italics (and alternative with roman)
  , "br" -- Newline
  , "TP" -- Process the next line as a "tag" and render all the following text
         -- as a paragraph, until we hit a new paragraph or section heading.
  , "TQ" -- This one is /not/ documented in man(7) but it seems to mean
         -- "process the next line as a part of the tag and start the paragraph
         --  on the next line instead."
  , "LP", "P", "PP" -- New paragraph
  ]

newtype PS = PS {unrecognized :: [Int]}

-- | Summary of rules gleamed from man.1 so far -
-- * @.B|.I@ - text is B/I but spaces are not underlined
-- * @.RB|.RI@ - text alternates between R and B/I separated by spaces
--               which are deleted
-- *
directiveLineP :: Int -> Parser ManPageLine
directiveLineP line_num = do
  dir <- takeWhile1P' (/= ' ')
  space
  txt <- case dir of
    "B" -> undefined
    "I" -> undefined
    "RB" -> undefined
    "RI" -> undefined
    "IR" -> undefined
    "BR" -> undefined
    "br" -> undefined
    "TP" -> undefined
    "TQ" -> undefined
    _ -> undefined
  let recognized = dir `Set.member` knownDirectives
  unless recognized
    $ modify (\ps -> ps{unrecognized = line_num : unrecognized ps})
  pure (Markup (Chunk (Dir dir) txt))

fullLine :: MonadParsec e s m => m (Tokens s)
fullLine = takeWhileP Nothing (const True)

manPageLineP :: Int -> Parser ManPageLine
manPageLineP line_num =
  (eof *> pure (Markup (Chunk None "")))
  <|> startsWithDotP
  <|> fallbackP
  where
    -- This T.cons seems hacky...
    fallbackP = Markup . Chunk None . T.cons ' ' <$> takeWhile1P' (const True)
    commentP = Comment <$> try (string "\\\"" *> fullLine)
    -- directiveLineP = do
    --   dir <- takeWhile1P' (not . isSpace)
    --   space
    startsWithDotP =
          try (char '\'' *> commentP)
      <|> (char '.' *>
           (    commentP
            <|> (Heading <$> try (lexeme (string "TH") *> manHeadingP))
            <|> directiveLineP line_num
            -- TODO: The following line doesn't make any sense.
            <|> fallbackP
            )
          )

data Chunk = Chunk !Directive !Text
  deriving Show

type Chunks = Vector Chunk

data ManPageLine
  = Markup  !Chunk
  | Comment !Text
  | Heading !Heading
  deriving Show

lexeme :: (MonadParsec e s f, Token s ~ Char) => f a -> f a
lexeme x = x <* space1

isAlphaNumOrDash :: Char -> Bool
isAlphaNumOrDash c = c == '-' || isAlphaNum c

binNameP :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
binNameP = takeWhile1P' isAlphaNumOrDash

secNumP :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
secNumP = takeWhile1P' isAlphaNumOrDash
