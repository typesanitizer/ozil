{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE TypeApplications    #-}

module Brick.FastMarkup
  ( FastMarkup
  , mkFastMarkup
  , fmWrapWith
  ) where

import Commons

import Brick (textWidth, Widget (..))
import Brick.Markup (GetAttr (..))
import Data.Functor.Identity (Identity (..))
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Monoid (Sum(..))
import Text.Wrap (WrapSettings (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Brick
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Graphics.Vty as Vty

data Emptiness
  = NE -- ^ Not Empty
  | PE -- ^ Perhaps Empty

data Entry (ne :: Emptiness) = Entry { _txt :: !Text, width :: !Int, attrIx :: !Int }

data FastMarkup a = FastMarkup !(Vector (Entry 'NE)) !(Vector a)

-- Postcondition: Entries have non-empty Text values.
mkFastMarkup :: Ord a => [(Text, a)] -> FastMarkup a
mkFastMarkup (filter (not . T.null . fst) -> tas) =
  let (_, as) = unzip tas
      as_list = Set.toList (Set.fromList as)
      as_map = Map.fromAscList (zip as_list [0 ..])
      tps = V.fromList [Entry t (textWidth t) i | (t, a) <- tas, let i = as_map Map.! a]
  in FastMarkup tps (V.fromList as_list)

-- | FastMarkup analog of txtWrapWith
fmWrapWith :: GetAttr a => WrapSettings -> FastMarkup a -> Widget n
fmWrapWith settings (FastMarkup tis as) =
  Widget Brick.Greedy Brick.Fixed $ do
    c <- Brick.getContext
    attrs <- traverse getAttr as
    let theLines = wrapLines settings (c ^. Brick.availWidthL) tis
        horizCat' = Vty.horizCat . V.toList
          . V.map (\(Entry t _ i :: Entry 'NE) ->
                     -- When your Haskell code looks like C code :(
                     let attr_i = if i == -1 then Vty.defAttr else attrs V.! i in
                     Vty.text' attr_i t
                  )
    pure $ case V.length theLines of
      0 -> Brick.emptyResult
      1 -> set Brick.imageL (horizCat' (theLines V.! 0)) Brick.emptyResult
      _ ->
        let lengths = V.map (V.sum . V.map width) theLines
            maxLen = V.maximum lengths
            lineImgs = V.imap lineImg theLines
            lineImg i line =
              let delta = maxLen - (lengths V.! i)
                  simpleImg = horizCat' line
                  padding = Vty.text' (c ^. Brick.attrL) (T.replicate delta " ")
              in if delta == 0 then simpleImg
                  else Vty.horizCat [simpleImg, padding]
        in set Brick.imageL (Vty.vertCat (V.toList lineImgs)) Brick.emptyResult

type Accum = Pair (Vector Token) (NonEmpty (Vector (Entry 'NE)))

wrapLines
  :: WrapSettings
  -> Int                -- ^ Wrapping width
  -> Vector (Entry 'NE)
  -> Vector (Vector (Entry 'NE))
wrapLines settings limit vs = V.unfoldr spill (Pair V.empty (vs :| []))
  where
    indent = V.span (T.all isSpace . _txt) vs
      & (\(wsEnts, notWsEnts) -> getSum
           $ foldMap (Sum . width) wsEnts
           <> fold @Maybe (Sum . textWidth . T.takeWhile isSpace . _txt
                           <$> V.headM notWsEnts))

    spill :: Accum -> Maybe (Vector (Entry 'NE), Accum)
    spill (Pair toks (e :| es)) =
      if V.length toks > 0 || V.length e > 0 then
        let (theLine, newToks, rest) = makeLine toks e
        in Just (theLine, Pair newToks (rest :| es))
      else case es of
        [] -> Nothing
        x : xs -> spill (Pair V.empty (x :| xs))

    makeLine :: Vector Token
             -> Vector (Entry 'NE)
             -> (Vector (Entry 'NE), Vector Token, Vector (Entry 'NE))
    makeLine tks ents =
      let (use_toks, new_ents) = if V.length tks > 0 then (tks, ents)
                                 else (tokenize (V.head ents), V.tail ents)
          (took, rest) = makeLineFromToks indent settings limit use_toks
      in (took, rest, new_ents)

data SplitWord = Split | Whole
  deriving Eq

padEntry :: Int -> Entry 'NE
padEntry w = assert (w > 0)
  $ Entry {_txt = T.replicate w " ", width = w, attrIx = -1} :: Entry 'NE

-- TODO: This function is too big...
makeLineFromToks
  :: Int
  -> WrapSettings
  -> Int
  -> Vector Token
  -> (Vector (Entry 'NE), Vector Token)
makeLineFromToks indent settings limit toks = (lhs, rhs)
  where
    remWidth = if preserveIndentation settings then limit - indent else limit

    consIndentation :: Vector (Entry 'NE) -> Vector (Entry 'NE)
    consIndentation =
      if preserveIndentation settings && indent > 0 then
      V.cons (padEntry indent)
      else id

    snocPadding :: Int -> Vector (Entry 'NE) -> Vector (Entry 'NE)
    snocPadding used_w = assert (used_w <= remWidth)
      $ if remWidth == used_w then id else flip V.snoc (padEntry (remWidth - used_w))

    go tw w si ei =
      if | ei == V.length toks || w == remWidth  -> (Whole, w, si, ei)
         -- Now ei < V.length toks
         -- Remove whitespace at the beginning of the line.
         | tw -> if | isWS (toks V.! ei) -> go True w (si + 1) (ei + 1)
                    | otherwise          -> go False w si ei
         -- tw is False now
         -- w < remWidth by induction
         | otherwise ->
           assert (w < remWidth)
           $ let w_i = width (entry (toks V.! ei)) in
             if | w + w_i <= remWidth -> go False (w + w_i) si (ei + 1)
                -- w + w_i > remWidth
                | ei == 0 && assert (w == 0 && si == 0) (w_i > remWidth) ->
                  (if breakLongWords settings then Split else Whole, remWidth, 0, 1)
                -- ei > 0
                | otherwise -> (Whole, w, si, ei)
    (splitFirstWord, usedW, start_i, end_i) = go True 0 0 0
    (lhs, rhs) = assert (end_i > 0) . assert (end_i > start_i)
      $ if splitFirstWord == Split
        then (let Entry{_txt,width,attrIx} = entry (toks V.! 0)
                  (t1, rem_w, t2) = splitAtWidth usedW _txt
                  e1 = Entry { _txt = t1 <> T.replicate rem_w " "
                             , width = usedW, attrIx } :: Entry 'NE
                  e2 = assert (not (T.null t2))
                       $ Entry { _txt = t2, width = width - usedW, attrIx }
                  hdTok = Token { isWS = False, entry = e2 }
              in (consIndentation $ V.singleton e1, V.cons hdTok (V.tail toks))
             )
        else (let (takeToks, remToks) =
                    V.splitAt (end_i - start_i) (V.drop start_i toks)
                  ents = V.map entry takeToks
             in (consIndentation $ snocPadding usedW ents, remToks)
             )

-- If the supplied width is too large, the second text will be empty.
splitAtWidth :: Int -> Text -> (Text, Int, Text)
splitAtWidth max_w t = (fromVec v_pre, max_w - used_w, fromVec v_post)
  where
    fromVec = T.pack . V.toList
    v :: UVector Char
    v = V.fromList $ T.unpack t
    vlen = V.length v
    go w i = if i == vlen then (w, i)
             else let w_i = textWidth (Identity (v V.! i))
                  in if | w + w_i > max_w -> (w, i)
                        | otherwise -> go (w + w_i) (i + 1)
    (used_w, ix) = go 0 0
    (v_pre, v_post) = V.splitAt ix v

data Token = Token { isWS :: !Bool, entry :: !(Entry 'NE) }

tokenize :: Entry 'NE -> Vector Token
tokenize = V.unfoldr $ \(Entry{_txt = t, width, attrIx} :: Entry 'NE) ->
  if T.null t
  then Nothing
  else let (isWS, (pre, post)) =
             if isSpace (T.head t)
             then (True, T.span isSpace t)
             else (False, T.span (not . isSpace) t)
           (pre_w, post_w) =
             if T.null post then (width, 0)
             else let pw = textWidth pre
                  in (pw, width - pw)
           (e1, e2) = (Entry{_txt = pre, width = pre_w, attrIx} :: Entry 'NE,
                       Entry{_txt = post, width = post_w, attrIx} :: Entry 'PE)
       -- Using unsafeCoerce here to change the type parameter is okay
       -- because, if the text is nonempty, we will return Nothing in the next
       -- iteration.
       in Just (Token{isWS, entry = e1}, unsafeCoerce e2 :: Entry 'NE)
