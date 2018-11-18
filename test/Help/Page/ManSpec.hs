{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Help.Page.ManSpec where

-- import Syntax.RawString

-- import Commons

-- import Help.Page.Man (Heading(..), manHeadingP)

-- import Data.Char
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Test.Hspec

-- spec_manHeadingP :: Spec
-- spec_manHeadingP = it "parse heading ok" $ do
--   parse @String manHeadingP "" l1 `shouldBe`
--       Right (ManHeading "GIT" "1" "10/05/2018" "Git 2\&.17\&.1" "Git Manual")
--   where
--     l1 = pack [raw|"GIT" "1" "10/05/2018" "Git 2\&.17\&.1" "Git Manual"\n|]
