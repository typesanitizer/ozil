{-# LANGUAGE QuasiQuotes     #-}

module Help.Page.Man.ParseSpec where

import Commons

import Syntax.RawString
import Help.Page.Man.Parse

import Text.Megaparsec
import Test.Hspec

tryParse :: Parsec () Text a -> Text -> Either PE a
tryParse p = parse p ""

shouldBeOk :: (Show (f a), Eq (f a), Applicative f) => f a -> a -> Expectation
shouldBeOk x y = shouldBe x (pure y)

spec_directiveArgP :: Spec
spec_directiveArgP = it "directive arg parsing" $ do
  go [r|"foo"|] `shouldBeOk` "foo"
  go [r|foo|]   `shouldBeOk` "foo"
  go [r|"   "|] `shouldBeOk` "   "
  go [r|\ \.|]  `shouldBeOk` " ."
  where
    go = tryParse directiveArgP

spec_lineP :: Spec
spec_lineP = it "line parsing" $ do
  go [r|.TH "foo" "bar" |] `shouldBeOk` ("TH", ["foo", "bar"])
  go [r|.X arg \"blah|] `shouldBeOk` ("X", ["arg"])
  where
    go = tryParse lineP
