module Data.Pair where

import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Pair a b = Pair !a !b
  deriving (Show, Eq, Ord, Bounded, Generic)

instance (Hashable a, Hashable b) => Hashable (Pair a b) where

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)

pairFst :: Pair a b -> a
pairFst (Pair !a !_b) = a

pairSnd :: Pair a b -> b
pairSnd (Pair !_a !b) = b

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair a b) = (a, b)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (a, b) = Pair a b
