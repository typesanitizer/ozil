{-# LANGUAGE DataKinds #-}

module Brick.FastMarkupSpec where

import Commons hiding ((===))
import Brick.FastMarkup

import Brick (textWidth)
import Data.Foldable (foldlM, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace (trace)
import Hedgehog
import Test.Hspec
import Test.Tasty
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

hprop_tokenize :: Property
hprop_tokenize = property $ do
  t <- forAll $ HG.text (HR.linear 1 100) (HG.choice [pure ' ', HG.alpha])
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
