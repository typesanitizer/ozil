{-# LANGUAGE MultiWayIf #-}

module Help.Ozil.App where

import Help.Ozil.App.Core
import Help.Ozil.App.Cmd
import Help.Ozil.App.Config

import Help.Page (DocPage)
import Control.Monad (void)
import Control.Monad.Extra (liftM2_1)

import qualified Help.Ozil.App.Default as Default

main :: IO ()
main = defaultMain runOzil

runOzil :: Options -> IO ()
runOzil opts =
  void
    .   liftM2_1 (execO opts) Default.config
    $   getConfig
    >>  selectPages
    >>= viewPages
    >>  saveConfig

selectPages :: O [DocPage]
selectPages = undefined
-- selectPage opts = do
--   print opts
--   exitSuccess
  -- manPages <- getManPages opts
  -- helpPage <- getHelpPage opts
  -- userSelection . catMaybes $ helpPage : manPages

-- getManPages :: Options -> IO DocPage
getManPages :: a
getManPages = undefined

getHelpPage :: a
getHelpPage = undefined

userSelection :: a
userSelection = undefined

viewPages :: [DocPage] -> O ()
viewPages = undefined
-- viewPage :: Options -> DocPage -> IO ()
-- viewPage = undefined
