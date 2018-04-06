module Help.Ozil.App where

import Control.Monad ((>=>))
import Help.Ozil.App.Cmd

main :: IO ()
main = defaultMain runOzil

runOzil :: Options -> IO ()
runOzil = print
