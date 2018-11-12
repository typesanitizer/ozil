{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Help.Page.Help where

import Commons

import Control.Lens (makeLenses)
import Control.Monad.State.Strict
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Control.Lens as L
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Debug.Trace as D

data HelpPage = HelpPage
  { _helpPageHeading  :: Optional
  , _helpPageSynopsis :: Optional -- ^ Equivalent to "usage"
  , _helpPageBody     :: Vector Item
  , _helpPageAnchors  :: UVector Int
  }

-- TODO: Maybe we should record offsets here?
data Item
  = Subcommand { _name :: Text, _description :: Text }
  | Flags      { _name :: Text, _description :: Text }
  | Plain Text
  deriving Show

data ItemIndent = ItemIndent { itemIndent :: !Int, descrIndent ::  !Int }
  deriving Show

data IndentGuess = IndentGuess
  { _flagIndent :: Maybe ItemIndent
  , _subcommandIndent :: Maybe ItemIndent
  } deriving Show
makeLenses ''IndentGuess

type Parser = ParsecT () Text (State IndentGuess)

-- getColumn :: ParsecT () Text (State IndentGuess) Int
getColumn :: MonadParsec e s m => m Int
getColumn = unPos . sourceColumn <$> getPosition

runHelpParser :: Parser a -> Text -> Either (ParseError Char ()) a
runHelpParser p txt = evalState (runParserT p "" txt) (IndentGuess Nothing Nothing)

eqIfJust :: Eq a => Maybe a -> a -> Bool
eqIfJust Nothing  _ = True
eqIfJust (Just y) x = x == y

twoColumn
  :: (MonadState s m, MonadParsec e Text m)
  => L.Lens' s (Maybe ItemIndent)
  -> (Text -> Text -> Item)
  -> m Text
  -> m Item
twoColumn lx ctor itemP = do
  -- Get first item
  space1
  itmCol <- getColumn
  saved <- L.use lx
  let itmIndent = fmap itemIndent saved
  guard (eqIfJust itmIndent itmCol)
  itm <- itemP
  -- Get description
  space1
  descCol <- getColumn
  let descIndent = fmap descrIndent saved
  guard (eqIfJust descIndent descCol)
  firstLine <- descrLine
  nextLines <- many $ do
    space1
    descCol' <- getColumn
    guard (descCol' == descCol)
    descrLine
  let descr = T.intercalate " " (firstLine : nextLines)
  -- Record positions and finish assembly
  when (isNothing saved)
    $ modify (set lx (Just (ItemIndent itmCol descCol)))
  pure (ctor itm descr)
  where
    -- Use lookAhead to avoid treating the synopsis as a subcommand.
    descrLine = lookAhead alphaNumChar *> takeWhile1P Nothing (/= '\n') <* newline

subcommandP :: Parser Item
subcommandP =
  twoColumn subcommandIndent Subcommand (takeWhile1P Nothing isAlphaNum)

singleLineP :: Parser Item
singleLineP = Plain <$>
  ( (T.singleton <$> char '\n')
    <|>
    (flip T.snoc '\n' <$> (takeWhile1P Nothing (/= '\n') <* optional newline))
  )

helpP :: Parser [Item]
helpP = some singleLineP <* optional eof

parsePickAnchors :: HasCallStack => Text -> (Vector Item, UVector Int)
parsePickAnchors t = (, V.empty)
  $ V.fromList
  $ (\case Right x -> x; Left y -> error (show y))
  $ runHelpParser helpP t
