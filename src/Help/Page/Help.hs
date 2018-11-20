{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Page.Help
  ( HelpPage (..), Item (..), ItemIndent (..), TableEntry (..), parseHelpPage
  , TableType (..), getEntry

  , IndentGuess (..), runHelpParser, helpP
  ) where

import Commons

import Help.Ozil.Death (unreachableError)

import Brick (textWidth)
import Control.Monad.State.Strict
import Data.Char (isSpace, isAlphaNum, isUpper)
import Data.List.Split (chop)
import Lens.Micro ((%~))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Lens', SimpleGetter)
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
    -- ^ Guessed indentation values. Note: The values contained are
    -- /not column numbers/ (indent = colNum - 1).
  }

getEntry :: Int -> HelpPage -> Maybe TableEntry
getEntry i HelpPage{_helpPageBody = b, _helpPageAnchors = a} =
  if i < 0 || i >= V.length a then Nothing
  else let (j, k) = a V.! i
       in Just (_entries (b V.! j) V.! k)

data TableType = Arg | Flag | Subcommand
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
  -- ^ Indentation for flags, with the second Int giving the indentation for a
  -- deeply indented flag, if there is one. For both
  -- @
  --   -f --foo      foo
  --      --bar      bar
  -- @
  -- and
  -- @
  --      --bar      bar
  --   -f --foo      foo
  -- @
  -- this value should be (Just 2 16, Just 5).
  , _subcommandIndent :: Maybe ItemIndent

  , _argIndent        :: Maybe ItemIndent
  -- ^ Stupid help pages like that of "rustup install" have different
  -- indentations for flag descriptions and arg descriptions. AAARGHH!!!!
  } deriving (Eq, Show)
makeLenses ''IndentGuess

--------------------------------------------------------------------------------
-- * Parsing

data PS = PS
  { _guess      :: !IndentGuess
  , _curPlainIndent :: !(Maybe Int)
  }
makeLenses ''PS

type Parser = ParsecT () Text (State PS)

runHelpParser :: Parser a -> Text -> (Either (ParseError Char ()) a, IndentGuess)
runHelpParser p txt = runState (runParserT p "" txt) initState & _2 %~ _guess
  where initState = PS (IndentGuess (Nothing, Nothing) Nothing Nothing) Nothing

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
  some (nl <|> try flagP <|> try argP <|> try subcommandP <|> plainP)
  <* optional eof
  where
    nl = Plain . T.singleton <$> newline

----------------------------------------------------------------------
-- ** Item parsers

------------------------------------------------------------
-- *** Plain text

plainP :: (MonadParsec e Text m, MonadState PS m) => m Item
plainP = do
  spaces <- takeWhileP Nothing isSpace
  ind <- getIndent
  modify (set curPlainIndent (Just ind))
  rest <- takeWhile1P' (/= '\n') <* optional newline
  pure $ Plain (T.snoc (spaces <> rest) '\n')

------------------------------------------------------------
-- *** Subcommands

subcommandP :: (MonadParsec e Text m, MonadState PS m) => m Item
subcommandP = simpleItemParser Subcommand subcommandTextP subcommandIndent

subcommandTextP :: (MonadParsec e Text m, MonadState PS m) => m Text
subcommandTextP = do
  pti <- gets (view curPlainIndent)
  curInd <- getIndent
  -- If we know we are in a text block, then it would better to assume that if
  -- we are sticking to the same indentation, we're still in the text block,
  -- instead of trying to parse the following as a subcommand.
  guard (pti /= Just curInd)
  lookAhead letterChar *> takeWhile1P' (\c -> c == '-' || isAlphaNum c)

------------------------------------------------------------
-- *** Flags

flagP :: (MonadParsec e Text m, MonadState PS m) => m Item
flagP =
  twoColumn
    Flag
    flagTextP
    (\s -> let (x, y) = s ^. flagIndent in fmap itemIndent x :| [y])
    (flagIndent . _1)
    (\itmInd descInd s -> case s ^. flagIndent of
        (Just _,  Just _)  -> pure ()
        (Nothing, Just _)  -> unreachableError
        (Nothing, Nothing) -> save _1 (Just (ItemIndent itmInd descInd))
        (Just fi, Nothing) ->
          let fi_ii = itemIndent fi
          in case compare fi_ii itmInd of
            EQ -> pure ()
            LT -> save _2 (Just itmInd)
            -- This case means we encountered a more deeply indented flag before
            -- encountering the shallow indented flag.
            GT -> save id (Just fi{itemIndent = itmInd}, Just fi_ii)
    )
   where save lx v = modify (set (guess . flagIndent . lx) v)

flagTextP :: MonadParsec e Text m => m Text
flagTextP = do
  firstFlag <- lookThenGobble (char '-')
  let next = try $ do
        space1
        lookThenGobble (satisfy (\c -> c == '[' || c == '<' || c == '-')
                        <|> (satisfy isUpper *> satisfy isUpper))
  nextStuff <- many next
  let flags = T.intercalate " " (firstFlag : nextStuff)
  pure flags

------------------------------------------------------------
-- *** Args

argP :: (MonadParsec e Text m, MonadState PS m) => m Item
argP = simpleItemParser Arg argTextP argIndent

argTextP :: MonadParsec e Text m => m Text
argTextP = lookThenGobble (char '<')

------------------------------------------------------------
-- *** Helper functions

simpleItemParser
  :: (MonadParsec e Text m, MonadState PS m)
  => TableType
  -> m Text
  -> Lens' IndentGuess (Maybe ItemIndent) -> m Item
simpleItemParser tblTy col1P indentLens =
  twoColumn
    tblTy
    col1P
    ((:| []) . fmap itemIndent . view indentLens)
    indentLens
    (\itmCol descInd s -> case s ^. indentLens of
        Just _  -> pure ()
        Nothing ->
          modify (set (guess . indentLens)
                  (Just (ItemIndent itmCol descInd)))
    )

lookThenGobble :: MonadParsec e Text m => m a -> m Text
lookThenGobble lk = lookAhead lk *> takeWhile1P' (/= ' ')

twoColumn
  :: (MonadParsec e Text m, MonadState PS m)
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
  s <- gets (view guess)
  let itmIndents = getSavedItemIndents s
  guard (any (`isNothingOrJustEq` itmInd) $ NE.toList itmIndents)
  itm <- itemP
  -- Now get the description
  spaces <- takeWhile1P' isSpace
  descInd <- getIndent
  desc <- descriptionP (textWidth spaces) (fmap descIndent (s ^. lx))
  -- Save indentations (if applicable)
  saveIndents itmInd descInd s
  modify (set curPlainIndent Nothing)
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
  => Int       -- ^ Preceding spaces
  -> Maybe Int -- ^ Indentation for description, if known.
  -> m Text
descriptionP precedingSpace savedDescIndent = do
  descInd <- getIndent
  guard (descriptionBlockIsDeeplyIndented descInd || precedingSpace > 1)
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
      lookAhead (notChar '[')
      *> takeWhile1P' (/= '\n')
      <* (void newline <|> eof)

isNothingOrJustEq :: Eq a => Maybe a -> a -> Bool
isNothingOrJustEq Nothing  _ = True
isNothingOrJustEq (Just x) y = x == y
