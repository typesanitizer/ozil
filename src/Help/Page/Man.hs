{-# LANGUAGE TypeFamilies    #-}

module Help.Page.Man
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
  ) where

import Commons

import Brick (AttrName)
import Brick.FastMarkup (FastMarkup, mkFastMarkup)
import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.HashSet (HashSet)
import Data.List.Split (chop)

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
  } deriving Eq

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
  } deriving Show

emptyHeading :: Heading
emptyHeading = ManHeading "" "" "" "" ""

manHeadingP :: MonadParsec e Text m => m Heading
manHeadingP = do
  _headingTitle <- lexeme binNameP
  _headingSection <- lexeme secNumP
  _headingDate <- lexeme quoted
  _headingSource <- lexeme quoted
  _headingManual <- quoted
  void (space *> optional eof)
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
  { _manPageViewHeading :: !Heading
  , _manPageViewSection :: !(Vector (Text, Chunks))
  , _manPageViewRest    :: !Text
  , _manPageFastMarkup  :: !(Vector (FastMarkup Brick.AttrName))
  }

data ManPageMetadata = ManPageMetadata
  { _manPageMetadataOriginal     :: !(Vector Text)
  , _manPageMetadataUnrecognized :: !(UVector Int)
  }

emptyManPage :: ManPage
emptyManPage = ManPage
  (ManPageView emptyHeading mempty "" undefined)
  (ManPageMetadata mempty mempty)

-- Ossanna and Kernighan's "Troff User's manual" seems to at least have the
-- comment syntax "correct" (it matches up with man.1). I guess I will just have
-- to fumble my way through this.
--
-- Any new insights should probably be documented on Unix.SE:
-- https://unix.stackexchange.com/q/481025/89474
parseManPage :: Text -> ManPage
parseManPage t =
  ManPage
  (ManPageView h sections t fm)
  (ManPageMetadata (V.fromList inp) (V.reverse $ V.fromList ps))
  where
    fm = V.singleton $ mkFastMarkup
      [ ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      , ("Twinkle twinkle little star, how I wonder what you are.", "subc-link")
      ]
    -- fm = V.map (mkFastMarkup . V.toList
    --             . fmap ((,"subc-link" :: AttrName) . (\(Chunk z _) -> z)) . snd)
    --      sections
    sections = mkSections rets'
    inp = T.lines t
    (rets, PS ps) =
      flip runState (PS [])
      $ traverse (\(i, line) -> runParserT (manPageLineP i) "" line)
      $ zip [0..] inp
    errmsg i v = printf "man page line %d parse error " i ++ show v
    -- TODO: Too many partial functions and incomplete matches here :(
    (tmp', tmp) = break isHeading
      $ zipWith (\i v -> fromRight (error (errmsg i v)) v) [1 :: Int ..] rets

    assertNoMoreHeadings xs = assert (not $ any isHeading xs) xs

    (Heading h, rets') = case tmp of
      (Heading h' : rest) -> (Heading h', assertNoMoreHeadings rest)
      _ -> error (show (take 20 tmp') ++ "\n" ++ show (take 10 tmp))

    isHeading = \case Heading _ -> True; _ -> False
    isSH = \case (Markup (Chunk  _ (Dir "SH"))) -> True; _ -> False

    getTxt (Markup (Chunk z _)) = z
    getTxt _ = error "unreachable"

    getChunks = catMaybes . chop groupConcat
    groupConcat [] = error "unreachable in chop"
    groupConcat (chk : chks) = case chk of
      Markup (Chunk txt d) ->
        let (grp, rest) = span (\case Markup (Chunk _ d') -> d' == d; _ -> False) chks in
          let txts = map getTxt grp in
          (Just (Chunk (T.intercalate " " (txt : txts)) d), rest)
      Comment _ -> (Nothing, chks)
      Heading _ -> error "Heading should not have existed here."

    mkSections :: [ManPageLine] -> Vector (Text, Chunks)
    mkSections = V.fromList . catMaybes . chop
      (\(l:ls) -> case l of
          Markup (Chunk sh (Dir "SH")) ->
            let (chks, rest) = break isSH ls
            in (Just (sh, V.fromList (getChunks chks)), rest)
          Markup (Chunk _ (Dir _)) -> error "Dir s. s /= \"SH\". \
                                            \This should've been gobbled by SH"
          Markup (Chunk _ None) -> error "Dir = None. \
                                         \This should've been gobbled by SH"
          Comment _ -> (Nothing, ls)
          Heading _ -> error "Heading should not have existed here."
      )

--------------------------------------------------------------------------------
-- * Helper functions

type Parser = ParsecT () Text (State PS)

data Directive = None | Dir !Text
  deriving (Eq, Show)

instance Semigroup Directive where
  None <> None = None
  None <> (Dir t) = Dir t
  Dir t <> None = Dir t
  (Dir _) <> (Dir t') = Dir t'

instance Monoid Directive where
  mempty = None

knownDirectives :: HashSet Text
knownDirectives = Set.fromList ["SH", "B", "RB", "RI", "IR", "I", "br"]

newtype PS = PS {unrecognized :: [Int]}

manPageLineP :: Int -> Parser ManPageLine
manPageLineP line_num =
  (eof *> pure (Markup (Chunk "" None)))
  <|> startsWithDotP
  <|> fallbackP
  where
    fallbackP = fmap Markup . Chunk <$> takeWhile1P' (const True) <*> pure None
    fullLine = takeWhileP Nothing (const True)
    commentP = Comment <$> try (string "\\\"" *> fullLine)
    startsWithDotP =
          try (char '\'' *> commentP)
      <|> (char '.' *>
           (    commentP
            <|> (Heading <$> try (lexeme (string "TH") *> manHeadingP))
            <|> do
               dir <- takeWhile1P' (/= ' ')
               txt <- fullLine
               let recognized = dir `Set.member` knownDirectives
               unless recognized
                 $ modify (\ps -> ps{unrecognized = line_num : unrecognized ps})
               pure (Markup (Chunk txt (Dir dir)))
            -- TODO: The following line doesn't make any sense.
            <|> fallbackP
            )
          )

data Chunk = Chunk !Text !Directive
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
