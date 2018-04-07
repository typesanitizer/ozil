module Help.Ozil.App where

import Data.Maybe (catMaybes)
import System.Exit (exitSuccess)

import Help.Page
import Help.Ozil.App.Core
import Help.Ozil.App.Cmd

main :: IO ()
main = defaultMain runOzil

runOzil :: Options -> IO ()
runOzil opts = selectPage opts >>= viewPage opts

getConfigFile :: Options -> O ()
getConfigFile = undefined

selectPage :: Options -> IO DocPage
selectPage opts = do
  print opts
  exitSuccess
  -- manPages <- getManPages opts
  -- helpPage <- getHelpPage opts
  -- userSelection . catMaybes $ helpPage : manPages

-- getManPages :: Options -> IO DocPage
getManPages = undefined

getHelpPage = undefined

userSelection = undefined

viewPage :: Options -> DocPage -> IO ()
viewPage = undefined
