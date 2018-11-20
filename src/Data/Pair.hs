module Data.Pair where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.List.Split (chop)
import GHC.Generics (Generic)

data Pair a b = Pair !a !b
  deriving (Show, Eq, Ord, Bounded, Generic, Functor)

instance (Hashable a, Hashable b) => Hashable (Pair a b) where

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)

pairFst :: Pair a b -> a
pairFst (Pair a _b) = a

pairSnd :: Pair a b -> b
pairSnd (Pair _a b) = b

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair a b) = (a, b)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (a, b) = Pair a b

coalesce :: (Foldable t, Eq a, Monoid b) => t (Pair a b) -> [Pair a b]
coalesce = coalesceWith mconcat

coalesceWith :: (Foldable t, Eq a) => ([b] -> b) -> t (Pair a b) -> [Pair a b]
coalesceWith concat_f x = toList x
  & chop (\(Pair ay by : ys) ->
            let (pre, post) = span (\p -> pairFst p == ay) ys
            in (Pair ay (concat_f (by : map pairSnd pre)), post)
         )
