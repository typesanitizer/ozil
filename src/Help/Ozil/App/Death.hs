module Help.Ozil.App.Death where

import Commons

oDie :: (HasCallStack, MonadIO m) => String -> m a
oDie = liftIO . error

unreachableErrorM :: (HasCallStack, MonadIO m) => m a
unreachableErrorM = oDie "Unreachable code!"

unreachableError :: HasCallStack => a
unreachableError = error "Unreachable code!"

unimplementedErrorM :: (HasCallStack, MonadIO m) => m a
unimplementedErrorM = oDie "Unimplemented :("

unimplementedError :: HasCallStack => a
unimplementedError = error "Unimplemented :("

fromJust' :: HasCallStack => Maybe a -> a
fromJust' = \case
  Just x  -> x
  Nothing -> unreachableError
