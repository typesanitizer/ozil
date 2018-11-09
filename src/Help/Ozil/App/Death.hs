module Help.Ozil.App.Death where

import Control.Monad.IO.Class (MonadIO (..))
import System.Exit (die)

oDie :: MonadIO m => String -> m a
oDie = liftIO . die

unreachableErrorM :: MonadIO m => m a
unreachableErrorM = oDie "Unreachable code!"

unreachableError :: a
unreachableError = error "Unreachable code!"

unimplementedErrorM :: MonadIO m => m a
unimplementedErrorM = oDie "Unimplemented :("

unimplementedError :: a
unimplementedError = error "Unimplemented :("
