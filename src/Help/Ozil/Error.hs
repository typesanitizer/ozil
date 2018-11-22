module Help.Ozil.Error where

import Control.Monad.IO.Class

import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Text.Megaparsec (ParseError)

data ParseFail s = ParseFail
  { inputSnippet :: s
  , message      :: Text
  , parseError   :: Maybe (ParseError Char ())
  }

annotate
  :: Text
  -> (s -> Either (ParseError Char ()) a)
  -> s
  -> Either (ParseFail s) a
annotate msg f s = case f s of
  Left x -> Left (ParseFail s msg (Just x))
  Right a -> Right a

mapMessage :: (Text -> Text) -> ParseFail s -> ParseFail s
mapMessage f pf = pf{message = f (message pf)}

--------------------------------------------------------------------------------
-- * Dying quickly.

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
