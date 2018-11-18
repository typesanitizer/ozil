{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Page.Lenses where

import Help.Page.Internal
import Help.Page.Help
import Help.Page.Man

import Lens.Micro.TH (makeFields, makeLenses)

makeFields ''HelpPage
makeFields ''Heading
makeFields ''ManPage
makeFields ''ManPageMetadata
makeFields ''ManPageView
makeFields ''DocPage
makeFields ''WhatisDescription
makeLenses ''HelpPageSummary
