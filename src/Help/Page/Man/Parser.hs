-- Oh God, what have I gotten myself into?
--
-- Ossanna and Kernighan's "Troff User's manual" seems to at least have the
-- comment syntax "correct" (it matches up with man.1). I guess I will just have
-- to fumble my way through this.
--
-- Any new insights should probably be documented on Unix.SE:
-- https://unix.stackexchange.com/q/481025/89474
module Help.Page.Man.Parser
  ( parseManPage
  , ParseError
  ) where

import Commons

import Help.Page.Man

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L

type RoffParseError = ()

type Parser = Parsec RoffParseError Text

parseManPage :: FilePath -> Text -> Either (ParseError Char RoffParseError) ManPage
parseManPage = parse manPageP

manPageP :: Parser ManPage
manPageP = undefined sc directive

sc :: Parser ()
sc = L.space space1 lineCmt empty
  where lineCmt = L.skipLineComment ".\\\""

directive :: Text -> Parser a -> Parser a
directive = undefined
