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

spec_charLiteral :: Spec
spec_charLiteral = it "char literal" $ do
  go [r|\fB|] `shouldBeOk` SwitchFont 3 Bold
  go' [r|\fBqu|] `shouldBeOk` [Pair Bold "q", Pair Bold "u"]
  where
    go = tryParse charLiteral
    go' = tryParse (many (directiveArgCharP (const False) "no-msg"))

spec_lineP :: Spec
spec_lineP = it "line parsing" $ do
  go [r|.TH "foo" "bar" |] `shouldBeOk` Directive "TH" ["foo", "bar"]
  go [r|.X arg \"blah|] `shouldBeOk` Directive "X" ["arg"]
  go [r|.BR abcd \-\-foo|] `shouldBeOk` Plain [Bold ~~ "abcd", Roman ~~ "--foo"]
  go [r|\fBqux\fP|] `shouldBeOk` Plain [Bold ~~ "qux"]
  go [r|\fBqux\fP \fIbar\fP|] `shouldBeOk`
     Plain [Bold ~~ "qux", Roman ~~ " ", Italic ~~ "bar"]
        -- ("", [[(Bold, "qux"), (Roman, " "), (Italic, "bar")]])
  where
    (~~) = Pair
    go = tryParse lineP
    -- go' = fmap (fmap (fmap (fmap pairToTuple))) . tryParse lineP
