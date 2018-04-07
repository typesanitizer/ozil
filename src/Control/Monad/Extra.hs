module Control.Monad.Extra where

import Control.Monad (join)

liftM2_1 :: Monad m => (a -> b -> m c) -> m a -> b -> m c
liftM2_1 f ma b = join $ f <$> ma <*> pure b
