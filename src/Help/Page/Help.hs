{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Page.Help
  ( HelpPage (..), Item (..), ItemIndent (..), TableEntry (..), parseHelpPage
  , TableType (..)
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
  , _helpPageSynopsis :: Optional           -- ^ Equivalent to "usage"
  , _helpPageBody     :: Vector Item
  , _helpPageAnchors  :: UVector (Int, Int)
    -- ^ Pair of coordinates to index into the body and _entries field.
  , _helpPageTableIxs :: UVector Int
    -- ^ List of indices at which tables are stored in body.
  }

data TableType = Flag | Subcommand
  deriving (Eq, Show)

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

data ItemIndent = ItemIndent { itemIndent :: !Int, descIndent ::  !Int }
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

-- TODO: We don't have Heading and synopsis parsing at the momemnt.
-- Do we need it? Can we do without it?
parseHelpPage :: HasCallStack => Text -> HelpPage
parseHelpPage txt =
  evalHelpParser helpP txt
  & (\case Right x -> x; Left y -> error (show y))
  & chop groupConcat
  & V.fromList
  & (\v ->
    let tixs = V.fromList
          [ i | (i, x) <- zip [0 ..] (V.toList v), isTabular Subcommand x ]
        ancs = V.unfoldr (go v tixs) (0, 0)
    in HelpPage Nothing Nothing v ancs tixs
    )
  where
    go v u (i, j) =
      if i >= V.length u
      then Nothing
      else let u_i = u V.! i
               v_u_i = _entries (v V.! u_i)
           in if j >= V.length v_u_i
              then go v u (i + 1, 0)
              else Just ((u_i, j), (i, j + 1))
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
    nl = Plain . T.singleton <$> newline

----------------------------------------------------------------------
-- ** Item parsers

------------------------------------------------------------
-- *** Plain text

plainP :: MonadParsec e Text m => m Item
plainP = Plain . flip T.snoc '\n'
  <$> (takeWhile1P Nothing (/= '\n') <* optional newline)

------------------------------------------------------------
-- *** Subcommands

subcommandP :: (MonadParsec e Text m, MonadState IndentGuess m) => m Item
subcommandP =
  twoColumn
    Subcommand
    subcommandTextP
    ((:| []) . fmap itemIndent . view subcommandIndent)
    subcommandIndent
    (\itmCol descCol s -> case s ^. subcommandIndent of
        Just _  -> pure ()
        Nothing ->
          modify (set subcommandIndent (Just (ItemIndent itmCol descCol)))
    )

subcommandTextP :: MonadParsec e Text m => m Text
subcommandTextP =
  lookAhead letterChar *> takeWhile1P Nothing (\c -> c == '-' || isAlphaNum c)

------------------------------------------------------------
-- *** Flags

flagP :: (MonadParsec e Text m, MonadState IndentGuess m) => m Item
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

flagTextP :: MonadParsec e Text m => m Text
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
    gobble :: MonadParsec e Text m => m a -> m Text
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
  desc <- descriptionP descCol (fmap descIndent (s ^. lx))
  -- Save indentations (if applicable)
  saveIndents itmCol descCol s
  -- Done
  pure (Tabular tt (pure (TableEntry itm desc)) (ItemIndent itmCol descCol))

descriptionP
  :: MonadParsec e Text m
  => Int       -- ^ Current column
  -> Maybe Int -- ^ Indentation for description, if known.
  -> m Text
descriptionP descCol descIndent = do
  guard (descCol >= 16)
  -- ^ OK, this one is a weird heuristic. The idea is that description blocks
  -- are usually deeply indented, so it would be odd if a description block
  -- was way over to the left. This helps us "catch" cases where you have
  -- paragraphs of indented text right at the beginning. Usually it will be in
  -- a sentence form, and the first word won't be that long (hopefully?).
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
