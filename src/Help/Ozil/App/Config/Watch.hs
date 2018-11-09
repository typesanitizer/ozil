module Help.Ozil.App.Config.Watch
  ( toReactOrNotToReact
  , module System.FSNotify
  )
  where

import System.FSNotify (Event (..), ActionPredicate)

import qualified Help.Ozil.App.Default as Default

-- | That is the question.
toReactOrNotToReact :: ActionPredicate
toReactOrNotToReact = \case
  Added    p _ False | p == cfgp -> True
  Modified p _ False | p == cfgp -> True
  Removed  p _ False | p == cfgp -> True
  Added{}                        -> False
  Modified{}                     -> False
  Removed{}                      -> False
  Unknown{}                      -> False
  where cfgp = Default.configPath
