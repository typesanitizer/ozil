{-# LANGUAGE DataKinds #-}

module Brick.FastMarkupSpec where

import Commons hiding ((===))
import Brick.FastMarkup

import Brick (textWidth)
import Text.Wrap
import Data.Foldable (foldlM, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace (trace)
import Hedgehog
import Test.Hspec
import Test.Tasty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

shortNonEmptyText = HG.text (HR.linear 1 100) (HG.choice [pure ' ', HG.alpha])

hprop_tokenize :: Property
hprop_tokenize = property $ do
  t <- forAll shortNonEmptyText
  let ent = Entry t (textWidth t) (-1) :: Entry 'NE
      toks = NE.fromList . V.toList $ tokenize ent
  void (alternatingWS toks)
  concatCheck t toks
  where
    alternatingWS (x :| xs) = do
      footnoteShow x
      footnoteShow xs
      foldlM (\b (_i, tok) -> isWS tok === not b >> pure (not b))
        (isWS x) (zip [1 :: Int ..] xs)
    concatCheck t xs = t === foldMap (_txt . entry) xs

textToEntry :: Text -> Entry a
textToEntry t = Entry {_txt = t, width = textWidth t, attrIx = -1}

entriesToText :: Foldable f => f (Entry a) -> Text
entriesToText = foldMap _txt

allEntriesToText :: (Functor f1, Foldable f1, Foldable f2) => f1 (f2 (Entry a)) -> Text
allEntriesToText = T.intercalate "\n" . toList . fmap entriesToText

hprop_wrapLineFit :: Property
hprop_wrapLineFit = property $ do
  t <- forAll $ HG.text (HR.linear 2 100) HG.alpha
  let t_len = T.length t
  brk <- forAll $ HG.int (HR.linear 1 (t_len - 1))
  let (pre, post) = T.splitAt brk t
  footnote (show (pre, post))
  wrapping t_len [pre, post] === t
  where
    wrapping = wrapConv settings
    settings = WrapSettings {preserveIndentation = False, breakLongWords = False}

wrapConv s n ts = allEntriesToText $ wrapLines s n (V.fromList (fmap textToEntry ts))

spec_wrapLines :: Spec
spec_wrapLines = it "wrap lines works" $ do
   wrapping 2 ["a", "a"] `shouldBe` "aa"
--   wrapping 3  ["ab cd "] `shouldBe` "ab \ncd "
--   wrapping 4  ["abcdef"] `shouldBe` "abcdef"

     -- wrapping 1 ["
--   wrapping 10 ["hello world"]        `shouldBe` "hello     \nworld     "
--   wrapping 10 ["hello", "world"]     `shouldBe` "helloworld"
--   wrapping 10 ["hello  ", "  world"] `shouldBe` "hello     \nworld     "

--   wrapping 10 ["abcdef", "gh jklmn"] `shouldBe` "abcdefgh  \njklmn     "
  where
--     wrapping' = wrapConv settings'
--     settings' = WrapSettings {preserveIndentation = False, breakLongWords = True}
    wrapping = wrapConv settings
    settings = WrapSettings {preserveIndentation = False, breakLongWords = False}
