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
  , module Data.Void
  , module GHC.Stack
  , readProcessSimple
  , headMaybe
  , pattern KeyPress
  )
  where

import Control.Exception (assert)
import Control.Lens ((^.), (^?), (<&>), view, set, over)
import Control.Lens.TH (makeFields)
import Control.Monad (when, void, join, forM, forM_)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Function ((&), on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Void
import GHC.Stack (HasCallStack)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))

import qualified Brick
import qualified Graphics.Vty as Vty

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
