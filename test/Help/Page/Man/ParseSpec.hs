{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes  #-}

module Help.Page.Man.ParseSpec where

import Commons

import Syntax.RawString
import Help.Page.Man.Parse

import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)
import Test.Hspec

tryParse :: ParsecT () Text (State PS) a -> Text -> Either PE a
tryParse p t = evalState (runParserT p "" t) initialPS

shouldBeOk :: (Show (f a), Eq (f a), Applicative f) => f a -> a -> Expectation
shouldBeOk x y = shouldBe x (pure y)

spec_directiveArgP :: Spec
spec_directiveArgP = it "directive arg parsing" $ do
  go [r|"foo"|] `shouldBeOk` "foo"
  go [r|foo|]   `shouldBeOk` "foo"
  go [r|"   "|] `shouldBeOk` "   "
  go [r|\ \.|]  `shouldBeOk` " ."
  where
    go = fmap forgetFont . tryParse directiveArgP

spec_lineP :: Spec
spec_lineP = it "line parsing" $ do
  go [r|.TH "foo" "bar" |] `shouldBeOk` ("TH", ["foo", "bar"])
  go [r|.X arg \"blah|] `shouldBeOk` ("X", ["arg"])
  where
    go = fmap (fmap (fmap forgetFont)) . tryParse lineP
