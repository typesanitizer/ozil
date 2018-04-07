{-# LANGUAGE TemplateHaskell #-}

module Help.Ozil.App.Config where

import Data.Set (Set)
import Data.Text (Text)

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Config = Config
  { helpByDefault :: Set Text
  , databasePath :: FilePath
  } deriving Show

$(deriveJSON defaultOptions ''Config)
