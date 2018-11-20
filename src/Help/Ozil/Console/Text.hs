module Help.Ozil.Console.Text
  ( warn
  , prompt
  , DefaultBool
  , pattern DefaultYes
  , pattern DefaultNo
  )
  where

import Commons

import System.IO (hFlush, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as T

warn :: Text -> IO ()
warn = T.putStrLn . T.append "Warning: "

newtype DefaultBool = DefaultBool Bool

{-# COMPLETE DefaultYes, DefaultNo #-}
pattern DefaultYes, DefaultNo :: DefaultBool
pattern DefaultYes = DefaultBool True
pattern DefaultNo  = DefaultBool False

prompt :: DefaultBool -> Text -> IO Bool
prompt def msg = do
  T.putStr fullMsg
  hFlush stdout
  print "Waiting for input..."
  inp <- T.getLine
  T.putStrLn (T.append "got " inp)
  case T.toLower (T.strip inp) of
    ""  -> pure (coerce def)
    "y" -> pure True
    "n" -> pure False
    _   -> T.putStrLn confusedMsg >> prompt def msg
  where
    confusedMsg = "Didn't quite get what you meant...\n\
                  \ Use one of y/Y/n/N or hit return to use the default value."
    fullMsg = T.append msg $ case def of
      DefaultYes -> " [Y/n] "
      DefaultNo  -> " [y/N] "
