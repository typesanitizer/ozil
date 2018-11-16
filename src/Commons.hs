{-# LANGUAGE TypeFamilies #-}

module Commons
  ( module Control.DeepSeq
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Coerce
  , module Data.Function
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Pair
  , module Data.Set
  , module Data.Text
  , module Data.Vector
  , UVector
  , module Data.Void
  , module GHC.Generics
  , module GHC.Stack
  , module Lens.Micro
  , module Lens.Micro.Mtl
  , module Lens.Micro.TH
  , module Text.Printf
  , readProcessSimple
  , headMaybe
  , inBounds
  , pattern KeyPress
  , Optional
  , (===)
  , (|||)
  , (!!!)
  , takeWhile1P'
  )
  where

import Control.DeepSeq (force, NFData(..))
import Control.Exception (assert)
import Lens.Micro ((^.), (^?), (<&>), set, over, _1, _2)
import Lens.Micro.Mtl (view)
import Lens.Micro.TH (makeFields)
import Control.Monad (when, void, join, forM, forM_)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Function ((&), on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Pair
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import Data.Void
import GHC.Generics (Generic (..))
import GHC.Stack (HasCallStack)
import Text.Printf (printf)
import Text.Megaparsec (MonadParsec, Tokens, Token, takeWhile1P)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))

import qualified Brick
import qualified Data.Vector.Generic as V
import qualified Graphics.Vty as Vty

type UVector = Data.Vector.Unboxed.Vector

readProcessSimple :: FilePath -> [String] -> IO (Maybe Text)
readProcessSimple p s = do
  (ec, out, _) <- readProcessWithExitCode p s ""
  pure $ case ec of
    ExitFailure _ -> Nothing
    ExitSuccess   -> Just (pack out)

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

inBounds :: Ord a => a -> a -> a -> a
inBounds mn v mx = max mn (min v mx)

pattern KeyPress :: Vty.Key -> Brick.BrickEvent n e
pattern KeyPress k = Brick.VtyEvent (Vty.EvKey k [])

type Optional = Maybe Text

(===) :: Brick.Widget n -> Brick.Widget n -> Brick.Widget n
(===) = (Brick.<=>)

(|||) :: Brick.Widget n -> Brick.Widget n -> Brick.Widget n
(|||) = (Brick.<+>)

takeWhile1P' :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
takeWhile1P' = takeWhile1P Nothing

(!!!) :: HasCallStack => V.Vector v a => v a -> Int -> a
(!!!) v i = fromMaybe err $ v V.!? i
  where err = error (printf "OOB indexing! i = %d, vlen = %d" i (V.length v))
