module Data.Focused
  ( Focused
  , singleton
  , fromNonEmpty
  , pop
  , tryPop
  , push
  , clipPush
  , clipPushBy
  , focus
  , focusL
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Lens.Micro.Type (Lens')

-- | A focused (list) is a non-empty list with an identified point.
--
-- @
--       Focus
-- 3 2 1 0 ↓ 0 1 2
-- A B C D E F G H
-- @
data Focused a = Focused [a] !a [a]
  deriving Functor

singleton :: a -> Focused a
singleton x = Focused [] x []

fromNonEmpty :: NonEmpty a -> Focused a
fromNonEmpty (x :| xs) = Focused [] x xs

-- | Try to extract the focused element.
--
-- The point of focus moves back by 1 if possible.
pop :: Focused a -> Maybe (a, Focused a)
pop (Focused []     _ []    ) = Nothing
pop (Focused []     x (y:ys)) = Just (x, Focused [] y ys)
pop (Focused (y:ys) x xs    ) = Just (x, Focused ys y xs)

-- | Try to drop the focused element.
tryPop :: Focused a -> Focused a
tryPop fc = maybe fc snd (pop fc)

-- | Push an element, moving the old focus behind the new focus.
push :: a -> Focused a -> Focused a
push x (Focused xs y zs) = Focused (y:xs) x zs

focus :: Focused a -> a
focus (Focused _ x _) = x

focusL :: Lens' (Focused a) a
focusL f (Focused ys x zs) = (\x' -> Focused ys x' zs) <$> f x

-- | Move the focus if the entry matches the next entry.
-- Otherwise, drop the tail and push the given entry.
--
-- This operation is like "going forward in history" in a browser. If you try
-- to go forward through another branch after going back, the existing
-- "forward history" is dropped.
clipPush :: Eq a => a -> Focused a -> Focused a
clipPush = clipPushBy (==)

-- | A more general version of 'clipPush'.
clipPushBy :: (a -> a -> Bool) -> a -> Focused a -> Focused a
clipPushBy f x (Focused ys y (x':xs)) | f x x' = Focused (y:ys) x xs
clipPushBy _ x (Focused ys y _)                = Focused (y:ys) x []

instance Foldable Focused where
  foldMap f (Focused pre x post) =
    foldMap f (reverse pre) <> f x <> foldMap f post
