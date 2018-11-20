{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

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
-- use of command to mean something else.

data PS = PS
  { connect :: !Bool
  , curFont :: !Font
  }

initialPS :: PS
initialPS = PS { connect = False, curFont = Roman }

type PE = ParseError Char ()

type MonadChars e s m = (MonadParsec e s m, Token s ~ Char)

type MonadRoff e s m = (MonadChars e s m, MonadState PS m)

nonspaceP :: MonadChars e s m => m (Tokens s)
nonspaceP = takeWhile1P' (not . isSpace)

data CharParseResult
  = Consume    { _consumeCount :: !Int, _parseResult :: !Text }
  | SwitchFont { _consumeCount :: !Int, _fontName :: !Font }
  | Connect

data Font = Bold | Italic | Roman
  deriving Eq

-- | Parser for man page characters handling escape codes.
--
-- Very loosely based off 'Text.Megaparsec.Char.Lexer.charLiteral'.
--
-- See page 7 in the Ossanna and Kernighan's Troff user manual for escape codes.
charLiteral :: MonadChars e s m => m CharParseResult
charLiteral = label "man page char literal" $ do
  -- The @~@ is needed to avoid requiring a MonadFail constraint,
  -- and we do know that r will be non-empty if count' succeeds.
  ~(x:y) <- lookAhead $ count' 1 4 C.anyChar
  case x of
    '\n' -> unexpected (Tokens ('\n':|[])) <?> "End of line"
    '\\' -> case y of
      [] -> unexpected (Tokens ('\\':|[])) <?> eoi_msg
      '\\':_ -> unexpected (Tokens (x:|y)) <?> unimpl_msg
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
      '(':z -> case z of
        [] -> unexpected (Tokens ('\\':|['('])) <?> eoi_msg
        "em" -> one 4 '—' -- Em dash (U+2014)
        "en" -> one 4 '–' -- En dash (U+2013)
        "hy" -> one 4 '-'
        "oq" -> one 4 '“' -- Left double quotation mark (U+201C), c.f. man.1
        "lq" -> one 4 '“' -- Left double quotation mark (U+201C), c.f. man.1
        "cq" -> one 4 '”' -- Right double quotation mark (U+201D), c.f. man.1
        "rq" -> one 4 '”' -- Right double quotation mark (U+201D), c.f. man.1
        "mu" -> one 4 '×' -- Multiplication sign (U+00D7)
        c:cs -> unexpected (Tokens (c:|cs))
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
    c -> one 1 c
  where
    one cnt c = pure (Consume cnt (T.singleton c))
    eoi_msg = "Unexpected end of input in the middle of an escape code."
    unimpl_msg = "Don't know how to handle this case yet."
    font_msg = "A font needs to be specified here."
    unknown_font_msg = "Got an unknown font :(, expected B, I, R or P."

consumeRestP :: MonadChars e s m => m a
consumeRestP =
  takeWhile1P' (/= '\n')
  *> unexpected (Tokens ('!':|[]))
  -- Finished consuming everything, fail now.
  <?> "Ate up the whole comment!"

space1NoNL :: MonadChars e s m => m ()
space1NoNL = void (takeWhile1P (Just "space no newline")
                   (\c -> c /= '\n' && isSpace c))

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme (L.space space1NoNL (L.skipLineComment "\\\"") empty)

type AnnText = Pair Font Text

forgetFont :: Foldable t => t AnnText -> Text
forgetFont = foldMap pairSnd

directiveArgCharP :: MonadRoff e s m => (Char -> Bool) -> String -> m AnnText
directiveArgCharP isBad msg = charLiteral >>= \case
  Consume 1 c | isBad (T.head c) -> unexpected (Tokens (T.head c :| [])) <?> msg
  Consume i t -> skipCount i C.anyChar *> annText t
  SwitchFont i f -> skipCount i C.anyChar *> switchFont f *> directiveArgCharP isBad msg
  Connect -> skipCount 2 C.anyChar *> turnConnectOn *> directiveArgCharP isBad msg
  where
    annText = (Pair <$> gets curFont <*>) . pure
    turnConnectOn = modify (\ps -> ps{connect = True})
    switchFont f = modify (\ps -> ps{curFont = f})

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
  c <- C.anyChar
  case c of
    '.' -> opt1
    '\'' -> undefined
    _ -> ( ,[]) <$> takeWhileP Nothing (/= '\n')
  where
    opt1 = do
      cmd  <- lexeme (takeWhile1P' (not . isSpace))
      args <- many (lexeme directiveArgP)
      pure (cmd, args)
