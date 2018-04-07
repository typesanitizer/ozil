module Help.Ozil.App.Console.Text where

import Data.Text (Text)
import System.IO (hFlush, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as T

warn :: Text -> IO ()
warn = T.putStrLn . T.append "Warning: "

prompt :: Bool -> Text -> IO Bool
prompt def msg = do
  T.putStr fullMsg
  hFlush stdout
  got <- T.getLine
  case T.toLower (T.strip got) of
    ""  -> pure def
    "y" -> pure True
    "n" -> pure False
    _ -> T.putStrLn confusedMsg >> prompt def msg
  where
    confusedMsg = "Didn't quite get what you meant...\n\
                  \ Use one of y/Y/n/N or hit return to use the default value."
    fullMsg = T.append msg $ if def then " [Y/n] " else " [y/N] "
