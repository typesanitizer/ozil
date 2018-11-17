{-# LANGUAGE RoleAnnotations #-}

module Help.Subcommand
  ( Subcommand
  , mkSubcommand
  )
  where

import Control.Exception (assert)

-- |
mkSubcommand :: String -> Subcommand
mkSubcommand s = assert (words s == [s]) (Subcommand s)

-- | String but with the assertion that everything inside is just 1 word.
newtype Subcommand = Subcommand String
  deriving Eq

instance Show Subcommand where
  show (Subcommand s) = s
