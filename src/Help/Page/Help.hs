{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Page.Help
  ( HelpPage (..), Item (..), ItemIndent (..), TableEntry (..), parsePickAnchors
  ) where

import Commons

import Help.Ozil.App.Death (unreachableError)

import Control.Lens (makeLenses)
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isUpper)
import Data.List.Split (chop)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Control.Lens as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector.Generic as V

data HelpPage = HelpPage
  { _helpPageHeading  :: Optional
  , _helpPageSynopsis :: Optional -- ^ Equivalent to "usage"
  , _helpPageBody     :: Vector Item
  , _helpPageAnchors  :: UVector Int
  }

data TableType = Flag | Subcommand
  deriving (Eq, Show)

-- TODO: Maybe we should record offsets here?
data Item
  = Plain Text
  | Tabular
    { _tableType :: !TableType
    , _entries   :: Vector TableEntry
    , _indents   :: !ItemIndent
    }
  deriving Show

data TableEntry = TableEntry { _name :: !Text, _description :: !Text }
  deriving Show

data ItemIndent = ItemIndent { itemIndent :: !Int, descrIndent ::  !Int }
  deriving Show

data IndentGuess = IndentGuess
  { _flagIndent :: (Maybe ItemIndent, Maybe Int)
  , _subcommandIndent :: Maybe ItemIndent
  } deriving Show
makeLenses ''IndentGuess

--------------------------------------------------------------------------------
-- * Parsing

type Parser = ParsecT () Text (State IndentGuess)

runHelpParser :: Parser a -> Text -> (Either (ParseError Char ()) a, IndentGuess)
runHelpParser p txt = runState (runParserT p "" txt) initState
  where initState = IndentGuess (Nothing, Nothing) Nothing

evalHelpParser :: Parser a -> Text -> Either (ParseError Char ()) a
evalHelpParser a b = fst (runHelpParser a b)

getColumn :: MonadParsec e s m => m Int
getColumn = unPos . sourceColumn <$> getPosition

-- TODO: Actually pick out indices for flags and subcommands.
parsePickAnchors :: HasCallStack => Text -> (Vector Item, UVector Int)
parsePickAnchors txt =
  evalHelpParser helpP txt
  & (\case Right x -> x; Left y -> error (show y))
  & chop groupConcat
  & V.fromList
  & (, V.empty)
  where
    -- This function should really be called esPlain ;)
    isPlain (Plain _) = True
    isPlain _ = False
    isTabular tt (Tabular tt' _ _) = tt == tt'
    isTabular _ _ = False
    groupConcat [] = (undefined, []) -- This case won't be called...
    groupConcat (itm : itms) = case itm of
      Plain t -> let (plains, rest) = span isPlain itms in
        (Plain (T.concat (t : map (\(Plain t') -> t') plains)), rest)
      Tabular tt tes _ -> let (tes', rest) = span (isTabular tt) itms in
        (itm {_entries = V.concat (tes : map _entries tes')}, rest)

helpP :: Parser [Item]
helpP =
  some (nl <|> try flagP <|> try subcommandP <|> plainP)
  <* optional eof
  where
    nl = Plain . T.singleton <$> char '\n'

----------------------------------------------------------------------
-- ** Item parsers

------------------------------------------------------------
-- *** Plain text

plainP :: Parser Item
plainP = Plain . flip T.snoc '\n'
  <$> (takeWhile1P Nothing (/= '\n') <* optional newline)

------------------------------------------------------------
-- *** Subcommands

subcommandP :: Parser Item
subcommandP =
  twoColumn
    Subcommand
    subcommandTextP
    ((:| []) . fmap itemIndent . view subcommandIndent)
    subcommandIndent
    (\itmCol descCol s -> case s ^. subcommandIndent of
        Nothing -> modify (set subcommandIndent (Just (ItemIndent itmCol descCol)))
        Just _  -> pure ()
    )

subcommandTextP :: Parser Text
subcommandTextP =
  lookAhead letterChar *> takeWhile1P Nothing (\c -> c == '-' || isAlphaNum c)

------------------------------------------------------------
-- *** Flags

flagP :: Parser Item
flagP =
  twoColumn
    Flag
    flagTextP
    (\s -> let (x, y) = s ^. flagIndent in fmap itemIndent x :| [y])
    (flagIndent . _1)
    (\itmCol descCol s -> case s ^. flagIndent of
        (Nothing, Nothing) -> save _1 (Just (ItemIndent itmCol descCol))
        (Just _,  Nothing) -> save _2 (Just itmCol)
        (Just _,  Just _)  -> pure ()
        (Nothing, Just _)  -> unreachableError
    )
   where save lx v = modify (set (flagIndent . lx) v)

flagTextP :: Parser Text
flagTextP = do
  first <- gobble (char '-')
  let next = try $ do
        space1
        gobble (satisfy (\c -> c == '[' || c == '<' || c == '-')
                <|> (satisfy isUpper *> satisfy isUpper))
  nextStuff <- many next
  let flags = T.intercalate " " (first : nextStuff)
  pure flags
  where
    gobble :: Parser a -> Parser Text
    gobble lk = lookAhead lk *> takeWhile1P Nothing (/= ' ')

------------------------------------------------------------
-- *** Helper functions

twoColumn
  :: (MonadParsec e Text m, MonadState IndentGuess m)
  => TableType                               -- ^ Item constructor
  -> m Text                                  -- ^ Item parser
  -> (IndentGuess -> NonEmpty (Maybe Int))   -- ^ Item alignments
  -> L.Getter IndentGuess (Maybe ItemIndent) -- ^ Getter for description indent
  -> (Int -> Int -> IndentGuess -> m ())     -- ^ Save state at the end.
  -> m Item
twoColumn tt itemP itmIndentsIn lx saveIndents = do
  -- First get the item
  space1
  itmCol <- getColumn
  s <- get
  let itmIndents = itmIndentsIn s
  guard (any (`isNothingOrJustEq` itmCol) $ NE.toList itmIndents)
  itm <- itemP
  -- Now get the description
  space1
  descCol <- getColumn
  descr <- descriptionP descCol (fmap descrIndent (s ^. lx))
  -- Save indentations (if applicable)
  saveIndents itmCol descCol s
  -- Done
  pure (Tabular tt (V.singleton (TableEntry itm descr)) (ItemIndent itmCol descCol))

descriptionP
  :: MonadParsec e Text m
  => Int       -- ^ Current column
  -> Maybe Int -- ^ Indentation for description, if known.
  -> m Text
descriptionP descCol descIndent = do
  guard (descIndent `isNothingOrJustEq` descCol)
  firstLine <- descrLine
  let nextLineP = try $ do
        space1
        descCol' <- getColumn
        guard (descCol' == descCol)
        descrLine
  nextLines <- many nextLineP
  pure (T.intercalate " " (firstLine : nextLines))
  where
    descrLine =
      lookAhead (notChar '[') *> takeWhile1P Nothing (/= '\n') <* newline

isNothingOrJustEq :: Eq a => Maybe a -> a -> Bool
isNothingOrJustEq Nothing  _ = True
isNothingOrJustEq (Just x) y = x == y
