module Control.Lens.Extra where

import Control.Monad.State.Class (MonadState)
import Control.Lens (ASetter, (<~))

-- | Restore symmetry to the universe.
(<~=) :: MonadState s m => ASetter s s a b -> m b -> m b
lx <~= mb = do lx <~ mb; mb
{-# INLINE (<~=) #-}
