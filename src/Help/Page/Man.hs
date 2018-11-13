{-# LANGUAGE TypeFamilies #-}

module Help.Page.Man
  ( WhatisDescription (..)
  , parseWhatisDescription
  , Heading (..)
  , ManPage (..)
  , emptyManPage
  , ManPageView (..)
  , ManPageMetadata (..)
  , parseManPage
  ) where

import Commons

import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.Char (isAlphaNum)
import Data.HashSet (HashSet)

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
  }

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
  }

emptyHeading :: Heading
emptyHeading = ManHeading "" "" "" "" ""

manHeadingP :: Parser Heading
manHeadingP = do
  directive "TH"
  _headingTitle <- lexeme binNameP
  _headingSection <- lexeme secNumP
  _headingDate <- lexeme quoted
  _headingSource <- lexeme quoted
  _headingManual <- lexeme quoted
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

-- | Structure of a typical man-page
-- See man-pages(7).
data ManPage = ManPage
  { _manPageView :: !ManPageView
  , _manPageMetadata :: !ManPageMetadata
  }

data ManPageView = ManPageView
  { _manPageViewHeading      :: !Heading
  , _manPageViewSection      :: !(Vector (Text, Text))
  , _manPageViewRest         :: !Text
  }

data ManPageMetadata = ManPageMetadata
  { _manPageMetadataOriginal     :: !(Vector Text)
  , _manPageMetadataUnrecognized :: !(UVector Int)
  }

emptyManPage :: ManPage
emptyManPage = ManPage (ManPageView emptyHeading mempty "") (ManPageMetadata mempty mempty)

parseManPage :: Text -> ManPage
parseManPage t = ManPage (ManPageView emptyHeading mempty t) (ManPageMetadata mempty mempty)
  -- let inp = T.lines tin
  --     (res, ps) = runManParser manPageP inp
  -- in ManPage (fromRight (error "man page parse failure!") res)
  --     ManPageMetadata  (V.fromList inp) (V.reverse (V.fromList $ unrecognized ps))


-- Ossanna and Kernighan's "Troff User's manual" seems to at least have the
-- comment syntax "correct" (it matches up with man.1). I guess I will just have
-- to fumble my way through this.
--
-- Any new insights should probably be documented on Unix.SE:
-- https://unix.stackexchange.com/q/481025/89474
manPageP :: Parser ManPageView
manPageP = undefined

--------------------------------------------------------------------------------
-- * Helper functions

type Parser = ParsecT () Text (State PS)

data Directive = None | Dir !Text

instance Semigroup Directive where
  None <> None = None
  None <> (Dir t) = Dir t
  Dir t <> None = Dir t
  (Dir _) <> (Dir t') = Dir t'

instance Monoid Directive where
  mempty = None

knownDirectives :: HashSet Text
knownDirectives = Set.fromList ["SH", "B", "RB", "RI", "IR", "I", "br"]

manPageLineP :: Parser ManPageLine
manPageLineP =
  startsWithDotP <|>
  (Markup <$> takeWhile1P' (/= '\n') <*> pure None)
  where
    startsWithDotP =
      (Comment <$> try (string "\\\"" *> takeWhile1P' (/= '\n')))
      <|> (Heading <$> try (string "TH" *> manHeadingP))
      <|> do
        dir <- takeWhile1P' (/= ' ')
        let recog = dir `Set.member` knownDirectives
        undefined

data ManPageLine
  = Markup  !Text !Directive
  | Comment !Text
  | Heading !Heading

runManParser = undefined
-- runManParser :: Parser a -> Text -> (Either (ParseError Char ()) a, PS)
-- runManParser p txt = runState (undefined p "" txt) initState
--   where initState = PS [] ""

data PS = PS
  { unrecognized     :: [Int]
  , currentDirective :: Directive
  }

directive :: MonadParsec e Text m => Text -> m ()
directive t = char '.' >> string t >> space1

lexeme :: (MonadParsec e s f, Token s ~ Char) => f a -> f a
lexeme x = x <* space1

isAlphaNumOrDash :: Char -> Bool
isAlphaNumOrDash c = c == '-' || isAlphaNum c

binNameP :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
binNameP = takeWhile1P' isAlphaNumOrDash

secNumP :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
secNumP = takeWhile1P' isAlphaNumOrDash

manSectionP :: Text -> Parser Text
manSectionP = undefined
