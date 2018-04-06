module Help.Ozil.App where

import Help.Page
import Help.Ozil.App.Cmd

main :: IO ()
main = defaultMain runOzil

runOzil :: Options -> IO ()
runOzil opts = getHelpPage opts >>= viewHelpPage opts

getHelpPage :: Options -> IO DocPage
getHelpPage = undefined

viewHelpPage :: Options -> DocPage -> IO ()
viewHelpPage = undefined
