{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Page.Lenses where

import Help.Page
import Help.Page.Help
import Help.Page.Man

import Data.Text (Text)
import Lens.Micro.TH (makeFields)

makeFields ''HelpPage
makeFields ''Heading
makeFields ''ManPage
makeFields ''ManPageMetadata
makeFields ''ManPageView
makeFields ''DocPage
makeFields ''WhatisDescription

instance HasRest ManPage Text where
  rest = view . rest
