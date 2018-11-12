module Commons
  ( module Control.Exception
  , module Control.Lens
  , module Control.Lens.TH
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Coerce
  , module Data.Function
  , module Data.List.NonEmpty
  , module Data.Maybe
  , module Data.Text
  , module Data.Vector
  , UVector
  , module Data.Void
  , module GHC.Stack
  , module Text.Printf
  , readProcessSimple
  , headMaybe
  , pattern KeyPress
  , Optional
  )
  where

import Control.Exception (assert)
import Control.Lens ((^.), (^?), (<&>), view, set, over, _1, _2)
import Control.Lens.TH (makeFields)
import Control.Monad (when, void, join, forM, forM_)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Function ((&), on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import Data.Void
import GHC.Stack (HasCallStack)
import Text.Printf (printf)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))

import qualified Brick
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

pattern KeyPress :: Vty.Key -> Brick.BrickEvent n e
pattern KeyPress k = Brick.VtyEvent (Vty.EvKey k [])

type Optional = Maybe Text
