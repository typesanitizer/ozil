module Data.Focused
  ( Focused
  , singleton
  , fromNonEmpty
  , pop
  , tryPop
  , push
  , clipPush
  , clipPushBy
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | A focused (list) is a list with an identified point.
--
-- @
--       Focused
-- 3 2 1 0 ↓ 0 1 2
-- A B C D E F G H
-- @
data Focused a = Focused [a] !a [a]
  deriving Functor

singleton :: a -> Focused a
singleton x = Focused [] x []

fromNonEmpty :: NonEmpty a -> Focused a
fromNonEmpty (x :| xs) = Focused [] x xs

pop :: Focused a -> Maybe (a, Focused a)
pop (Focused []     _ []    ) = Nothing
pop (Focused []     x (y:ys)) = Just (x, Focused [] y ys)
pop (Focused (y:ys) x xs    ) = Just (x, Focused ys y xs)

tryPop :: Focused a -> Focused a
tryPop fc = maybe fc snd (pop fc)

push :: a -> Focused a -> Focused a
push x (Focused xs y zs) = Focused (y:xs) x zs

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
  foldMap f (Focused pre x post) = foldMap f (reverse pre) <> f x <> foldMap f post

-- Not sure if this is lawful?
-- instance Traversable Focused where
--   traverse f (Focused pre x post) = Focused <$> traverse f pre <*> f x <*> traverse f post
