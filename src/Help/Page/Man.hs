module Help.Page.Man
  ( WhatisDescription (..)
  , parseWhatisDescription
  , Heading (..)
  , ManPage (..)
  , emptyManPage
  , ManPageView (..)
  , ManPageMetadata (..)
  , Chunk (Chunk)
  , parseManPage
  , manHeadingP
  ) where

import Help.Page.Man.Internal
