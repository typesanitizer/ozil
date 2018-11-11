{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Help.Page.Lenses where

import Help.Page
import Help.Page.Help
import Help.Page.Man

import Control.Lens.TH (makeFields)

makeFields ''HelpPage
makeFields ''Heading
makeFields ''ManPage
makeFields ''DocPage
makeFields ''WhatisDescription
