{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Help.Page.ManSpec where

import Syntax.RawString

import Commons

import Help.Page.Man

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Test.Hspec

parseDL :: Text -> Either (Either PE ManPageLine) Chunk
parseDL t = runManParser p' t & fst & \case
  Right (Markup c) -> Right c
  Left pe -> Left (Left pe)
  Right x -> Left (Right x)
  where
    p' = char '.' *> directiveLineP 0

spec_directiveLine :: Spec
spec_directiveLine = it "parse directive" $ do
  parseDL [r|.B \-t ,|] `shouldBe` Right (Chunk (Dir "B") "-t

-- spec_manHeadingP :: Spec
-- spec_manHeadingP = it "parse heading ok" $ do
--   parse @String manHeadingP "" l1 `shouldBe`
--       Right (ManHeading "GIT" "1" "10/05/2018" "Git 2\&.17\&.1" "Git Manual")
--   where
--     l1 = pack [raw|"GIT" "1" "10/05/2018" "Git 2\&.17\&.1" "Git Manual"\n|]
