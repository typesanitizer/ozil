{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Syntax.RawString where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

raw :: QuasiQuoter
raw = QuasiQuoter
  { quoteExp  = pure . LitE . StringL . fixNewlines
  , quotePat  = \_ -> fail "Illegal use of raw string as pattern"
  , quoteType = \_ -> fail "Illegal use of raw string as type"
  , quoteDec  = \_ -> fail "Illegal use of raw string as declaration"
  }

fixNewlines :: String -> String
fixNewlines []             = []
fixNewlines ('\r':'\n':cs) = '\n':fixNewlines cs
fixNewlines (c:cs)         = c:fixNewlines cs
