module Control.Monad.Extra where

import Control.Monad (liftM2, join)

liftM2_1 :: Monad m => (a -> b -> m c) -> m a -> b -> m c
liftM2_1 f ma = join . liftM2 f ma . pure
