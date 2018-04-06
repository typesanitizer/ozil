module Help.Ozil.App.Cmd
  ( Options
  , options
  , defaultMain
  )
  where

import Data.Semigroup
import Options.Applicative

defaultMain :: (Options -> IO b) -> IO b
defaultMain runOzil = execParser opts >>= runOzil
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> header "ozil - Frictionless browsing of man/help pages."
    <> progDesc
         "ozil assists you with viewing man/help pages. It is intended as a replacement to man/--help + less/more/most."
    )

data Options = Options
  { autofind :: Bool
  , cmdname :: [String]
  }
  deriving Show

offSwitch :: Mod FlagFields Bool -> Parser Bool
offSwitch = fmap not . switch

options :: Parser Options
options =
  Options
    <$> offSwitch
          (  long "no-autofind"
          <> help "Turn off intelligent searching if binary is missing."
          )
    <*> cmdnameP
  where cmdnameP = some . strArgument $ metavar "<cmd>"
