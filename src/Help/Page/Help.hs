{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Page.Help
  ( HelpPage (..), Item (..), ItemIndent (..), TableEntry (..), parseHelpPage
  , TableType (..), getEntry

  , IndentGuess (..), runHelpParser, helpP
  ) where

import Commons

import Help.Ozil.App.Death (unreachableError)

import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isUpper)
import Data.List.Split (chop)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (SimpleGetter)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector.Generic as V

data HelpPage = HelpPage
  { _helpPageHeading  :: Optional
  , _helpPageSynopsis :: Optional           -- ^ Equivalent to "usage"
  , _helpPageBody     :: Vector Item
  , _helpPageAnchors  :: UVector (Int, Int)
    -- ^ Pair of coordinates to index into the body and then Item._entries.
    -- E.g. if the vector is [(3, 0), (3, 1), (7, 0), (7, 1) (7, 2)] that
    -- means that we have two tables of subcommands, stored at indices 3 and 7,
    -- and the first one has two elements and the second one has 3 elements.
    -- It is useful to store all pairs and not just lengths, because now we
    -- can index+iterate directly -- e.g. we now know that there are 5
    -- subcommands and the fifth one is at (7, 2).
  , _helpPageTableIxs :: UVector Int
    -- ^ Indices at which subcommand tables are stored in body.
  , _helpPageIndents  :: IndentGuess
    -- ^ Guessed indentation values.
  }

getEntry :: Int -> HelpPage -> Maybe TableEntry
getEntry i HelpPage{_helpPageBody = b, _helpPageAnchors = a} =
  if i < 0 || i >= V.length a then Nothing
  else let (j, k) = a V.! i
       in Just (_entries (b V.! j) V.! k)

data TableType = Flag | Subcommand
  deriving (Eq, Show)

data Item
  = Plain Text
  | Tabular
    { _tableType :: !TableType
    , _entries   :: Vector TableEntry
    , _indents   :: !ItemIndent
    }
  deriving (Eq, Show)

data TableEntry = TableEntry { _name :: !Text, _description :: !Text }
  deriving (Eq, Show)

data ItemIndent = ItemIndent { itemIndent :: !Int, descIndent ::  !Int }
  deriving (Eq, Show)

data IndentGuess = IndentGuess
  { _flagIndent :: (Maybe ItemIndent, Maybe Int)
  , _subcommandIndent :: Maybe ItemIndent
  } deriving (Eq, Show)
makeLenses ''IndentGuess

--------------------------------------------------------------------------------
-- * Parsing

type Parser = ParsecT () Text (State IndentGuess)

runHelpParser :: Parser a -> Text -> (Either (ParseError Char ()) a, IndentGuess)
runHelpParser p txt = runState (runParserT p "" txt) initState
  where initState = IndentGuess (Nothing, Nothing) Nothing

getIndent :: MonadParsec e s m => m Int
getIndent = subtract 1 . unPos . sourceColumn <$> getPosition

-- TODO: We don't have Heading and synopsis parsing at the momemnt.
-- Do we need it? Can we do without it?
parseHelpPage :: HasCallStack => Text -> HelpPage
parseHelpPage txt =
  runHelpParser helpP txt
  & (\(items, indents) ->
    items & (\case Right x -> x; Left y -> error (show y))
          & chop groupConcat
          & V.fromList
          & (\v ->
               let tixs =
                     V.fromList [ i | (i, x) <- zip [0 ..] (V.toList v)
                                    , isTabular Subcommand x ]
                   ancs = V.unfoldr (go v tixs) (0, 0)
               in HelpPage Nothing Nothing v ancs tixs indents
            )
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
    (\itmCol descInd s -> case s ^. subcommandIndent of
        Just _  -> pure ()
        Nothing ->
          modify (set subcommandIndent (Just (ItemIndent itmCol descInd)))
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
    (\itmCol descInd s -> case s ^. flagIndent of
        (Nothing, Nothing) -> save _1 (Just (ItemIndent itmCol descInd))
        (Just _,  Nothing) -> save _2 (Just itmCol)
        (Just _,  Just _)  -> pure ()
        (Nothing, Just _)  -> unreachableError
    )
   where save lx v = modify (set (flagIndent . lx) v)

flagTextP :: MonadParsec e Text m => m Text
flagTextP = do
  firstFlag <- gobble (char '-')
  let next = try $ do
        space1
        gobble (satisfy (\c -> c == '[' || c == '<' || c == '-')
                <|> (satisfy isUpper *> satisfy isUpper))
  nextStuff <- many next
  let flags = T.intercalate " " (firstFlag : nextStuff)
  pure flags
  where
    gobble :: MonadParsec e Text m => m a -> m Text
    gobble lk = lookAhead lk *> takeWhile1P Nothing (/= ' ')

------------------------------------------------------------
-- *** Helper functions

twoColumn
  :: (MonadParsec e Text m, MonadState IndentGuess m)
  => TableType
  -> m Text                                      -- ^ Item parser
  -> (IndentGuess -> NonEmpty (Maybe Int))       -- ^ Item alignments
  -> SimpleGetter IndentGuess (Maybe ItemIndent) -- ^ Get description indent
  -> (Int -> Int -> IndentGuess -> m ())         -- ^ Save state at the end.
  -> m Item
twoColumn tt itemP getSavedItemIndents lx saveIndents = do
  -- First get the item
  space1
  itmInd <- getIndent
  s <- get
  let itmIndents = getSavedItemIndents s
  guard (any (`isNothingOrJustEq` itmInd) $ NE.toList itmIndents)
  itm <- itemP
  -- Now get the description
  space1
  descInd <- getIndent
  desc <- descriptionP descInd (fmap descIndent (s ^. lx))
  -- Save indentations (if applicable)
  saveIndents itmInd descInd s
  -- Done
  pure (Tabular tt (pure (TableEntry itm desc)) (ItemIndent itmInd descInd))

-- | OK, this one is a weird heuristic. The idea is that description blocks
-- are usually deeply indented, so it would be odd if a description block
-- was way over to the left. This helps us "catch" cases where you have
-- paragraphs of indented text right at the beginning. Usually it will be in
-- a sentence form, and the first word won't be that long (hopefully?).
descriptionBlockIsDeeplyIndented :: Int -> Bool
descriptionBlockIsDeeplyIndented = (>= 16)

descriptionP
  :: MonadParsec e Text m
  => Int       -- ^ Current column
  -> Maybe Int -- ^ Indentation for description, if known.
  -> m Text
descriptionP descInd savedDescIndent = do
  guard (descriptionBlockIsDeeplyIndented descInd)
  guard (savedDescIndent `isNothingOrJustEq` descInd)
  firstLine <- descrLine
  let nextLineP = try $ do
        space1
        descInd' <- getIndent
        guard (descInd' == descInd)
        descrLine
  nextLines <- many nextLineP
  pure (T.intercalate " " (firstLine : nextLines))
  where
    descrLine =
      lookAhead (notChar '[') *> takeWhile1P Nothing (/= '\n') <* (void newline <|> eof)

isNothingOrJustEq :: Eq a => Maybe a -> a -> Bool
isNothingOrJustEq Nothing  _ = True
isNothingOrJustEq (Just x) y = x == y
