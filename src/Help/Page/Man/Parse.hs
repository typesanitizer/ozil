{-# LANGUAGE ConstraintKinds #-}

-- | The one deadly module.
--
-- We use the abbreviation TUM for Ossanna and Kernighan's Troff User's Manual.
-- https://www.troff.org/54.pdf
module Help.Page.Man.Parse where

import Commons

import Data.Char (isSpace)

import Control.Monad.State.Strict
import Text.Megaparsec

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- * Groff macros (based off man(7)) with troff
--
-- We use the term "directive" instead of command to avoid confusion with the
-- use of command to mean something else in the context of the application.

data PS = PS
  { connect :: !Bool
  , ignore  :: !(Maybe Text) -- .ig yy (default yy = ".."). See TUM pg 6.
  , curFont :: !Font
  }

initialPS :: PS
initialPS = PS { connect = False, ignore = Nothing, curFont = Roman }

type PE = ParseError Char ()

type MonadChars e s m = (MonadParsec e s m, Token s ~ Char)

type MonadRoff e s m = (MonadChars e s m, MonadState PS m)

nonspaceP :: MonadChars e s m => m (Tokens s)
nonspaceP = takeWhile1P' (not . isSpace)

data CharParseResult
  = Consume    { _consumeCount :: !Int, _parseResult :: !Text }
  | SwitchFont { _consumeCount :: !Int, _fontName :: !Font }
  | Connect
  deriving (Eq, Show)

data Font = Bold | Italic | Roman
  deriving (Eq, Show)

data OpenBracket = Paren | Square

eoiMsg :: String
eoiMsg = "Unexpected end of input in the middle of an escape code."

-- | The bracket escape codes are partly derived from TUM. The behaviour for
-- square brackets is guessed based off the rendering of groff.1.
bracketCase :: MonadChars e s m => OpenBracket -> m CharParseResult
bracketCase brkt = label "in bracket char" $ do
  ~(_:_:tl) <- lookAhead $ count' 4 5 C.anyChar
  case tl of
    [] -> unexpected (Tokens ('\\':|['('])) <?> eoiMsg
    'e':'m':t -> one t '—' -- Em dash (U+2014)
    'e':'n':t -> one t '–' -- En dash (U+2013)
    'h':'y':t -> one t '-'
    'o':'q':t -> one t '“' -- Left double quotation mark (U+201C), see man.1
    'l':'q':t -> one t '“' -- Left double quotation mark (U+201C), see man.1
    'c':'q':t -> one t '”' -- Right double quotation mark (U+201D), see man.1
    'r':'q':t -> one t '”' -- Right double quotation mark (U+201D), see man.1
    'm':'u':t -> one t '×' -- Multiplication sign (U+00D7)
    'c':'o':t -> one t '©' -- Copyright sign (U+00A9)
    c:cs -> unexpected (Tokens (c:|cs))
  where
    one t c = case brkt of
      Paren -> pure (Consume 4 (T.singleton c))
      Square -> case t of
        [']'] -> pure (Consume 5 (T.singleton c))
        _ -> unexpected (Tokens ('\\':|(['[', '.', '.'] ++ t)))

-- | Parser for man page characters handling escape codes.
--
-- Very loosely based off 'Text.Megaparsec.Char.Lexer.charLiteral'.
--
-- See TUM (page 7) for escape codes.
charLiteral :: MonadChars e s m => m CharParseResult
charLiteral = label "man page char literal" $ do
  -- The @~@ is needed to avoid requiring a MonadFail constraint,
  -- and we do know that r will be non-empty if count' succeeds.
  ~(x:y) <- lookAhead $ count' 1 3 C.anyChar
  case x of
    '\n' -> unexpected (Tokens ('\n':|[])) <?> "End of line"
    '\\' -> case y of
      [] -> unexpected (Tokens ('\\':|[])) <?> eoiMsg
      '\\':_ -> unexpected (Tokens (x:|y)) <?> unimpl_msg
      -- seems to mean a suggestion for the line-breaking suggest algorithm.
      -- see in a URL in groff(1)
      ':':_ -> pure (Consume 2 "")
      '`':_ -> one 2 '`'
      '\'':_ -> one 2 '´'
      '.':_ -> one 2 '.'
      '-':_ -> one 2 '-'
      ' ':_ -> one 2 ' '
      '0':_ -> one 2 ' ' -- Assume digit width space = normal space
      '|':_ -> one 2 ' ' -- Six-per-em space (U+2006)
      '^':_ -> one 2 ' ' -- Hair space (U+200A)
      '&':_ -> one 2 ' ' -- GHC doesn't support zero width space (U+200B) in
                         -- literals so we use Hair space again
      '!':_ -> unexpected (Tokens ('!':|[])) <?> unimpl_msg
      '%':_ -> one 2 '-' -- hypenation using -
      -- '"':_ -> takeWhile1P' (const True) *> done
      '(':_ -> bracketCase Paren
      '[':_ -> bracketCase Square
      'c':_ -> pure Connect
      'e':_ -> one 2 '\\' -- This is supposed to be the "current"
                          -- escape character, whatever that means.
      'f':z -> case z of
        -- O&K also suggests other forms of \f with font numbers but we ignore
        -- those for now.
        [] -> unexpected (Tokens ('\\':|['f'])) <?> font_msg
        'B':_ -> pure (SwitchFont 3 Bold)
        'I':_ -> pure (SwitchFont 3 Italic)
        'P':_ -> pure (SwitchFont 3 Roman) -- TODO: where is this specified
        'R':_ -> pure (SwitchFont 3 Roman)
        _ -> unexpected (Tokens ('\\':|('f':z))) <?> unknown_font_msg
    c -> one 1 c -- (if c == 'f' then error "f!" else one 1 c)
  where
    one cnt c = pure (Consume cnt (T.singleton c))
    unimpl_msg = "Don't know how to handle this case yet."
    font_msg = "A font needs to be specified here."
    unknown_font_msg = "Got an unknown font :(, expected B, I, R or P."

space1NoNL :: MonadChars e s m => m ()
space1NoNL = void (takeWhile1P (Just "space no newline")
                   (\c -> c /= '\n' && isSpace c))

data NewlineResult = Leave1Space | NoSpace

newline :: MonadRoff e s m => m NewlineResult
newline = C.newline >> do
  PS{connect} <- get
  put initialPS
  pure (if connect then NoSpace else Leave1Space)

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme (L.space space1NoNL (L.skipLineComment "\\\"") empty)

type AnnText = Pair Font Text

forgetFont :: Foldable t => t AnnText -> Text
forgetFont = foldMap pairSnd

directiveArgCharP :: MonadRoff e s m => (Char -> Bool) -> String -> m AnnText
directiveArgCharP isBad msg = charLiteral >>= \case
  Consume 1 c | isBad (T.head c) -> unexpected (Tokens (T.head c :| [])) <?> msg
  Consume i t -> skipCount i C.anyChar *> annText t
  SwitchFont i f -> skipCount i C.anyChar *> switchFont f *>
    (directiveArgCharP isBad msg <|> annText "")
  Connect -> skipCount 2 C.anyChar *> turnConnectOn *>
    (directiveArgCharP isBad msg <|> annText "")
  where
    annText = (Pair <$> gets curFont <*>) . pure
    turnConnectOn = modify (\ps -> ps{connect = True})
    switchFont f =
      -- if f == Bold then error "blowup switchFont"
      -- else error ("blowup! font = " ++ show f)
      modify (\ps -> ps{curFont = f})

directiveArgChar1P :: MonadRoff e s m => m AnnText
directiveArgChar1P = directiveArgCharP (== '"') "Got double quote"

directiveArgChar2P :: MonadRoff e s m => m AnnText
directiveArgChar2P = directiveArgCharP isSpace "Got space char"

-- Since directive arguments are typically expected to use the same font
-- throughout, the lists will be small.
directiveArgP :: MonadRoff e s m => m [AnnText]
directiveArgP =
  between (C.char '"') (C.char '"') (coalesce <$> many directiveArgChar1P)
  <|> (coalesce <$> some directiveArgChar2P)

type Words a = [a]

lineP :: MonadRoff e Text m => m (Text, Words [AnnText])
lineP = do
  c <- lookAhead C.anyChar
  case c of
    '.' -> skipCount 1 C.anyChar *> opt1
            -- Skip these as preprocessor is handled separately
    '\'' -> skipCount 1 C.anyChar *> pure ("", [])
    _ -> ("",) . (:[]) . coalesce . filter (not . T.null . pairSnd) <$> lineP'
  where
    lineP' = many (directiveArgCharP (const False) "Unreachable")
    opt1 = do
      cmd  <- lexeme (takeWhile1P' (not . isSpace))
      args <- many (lexeme directiveArgP)
      pure (cmd, args)

preprocessorLineP :: MonadRoff e Text m => m Text
preprocessorLineP = C.string "'\\\""
  *> (T.strip <$> takeWhile1P Nothing (/= '\n'))
