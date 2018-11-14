{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

module Brick.FastMarkup
  ( FastMarkup
  , mkFastMarkup
  ) where

import Commons

import Brick (textWidth, Widget (..))
import Brick.Markup (GetAttr (..))
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Monoid (Sum(..))
import Text.Wrap (WrapSettings (..))

import qualified Brick
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Generic as V
import qualified Graphics.Vty as Vty

data Entry = Entry { _txt :: !Text, width :: !Int, attrIx :: !Int }

data FastMarkup a = FastMarkup !(Vector Entry) !(Vector a)

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
          . V.map (\(Entry t _ i) -> Vty.text' (attrs V.! i) t)
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

type Accum = Pair (Vector Token) (NonEmpty (Vector Entry))

wrapLines
  :: WrapSettings
  -> Int          -- ^ Wrapping width
  -> Vector Entry
  -> Vector (Vector Entry)
wrapLines settings limit vs = V.unfoldr spill (Pair V.empty (vs :| []))
  where
    indent = V.span (T.all isSpace . _txt) vs
      & (\(wsEnts, notWsEnts) -> getSum
           $ foldMap (Sum . width) wsEnts
           <> fold @Maybe (Sum . textWidth . T.takeWhile isSpace . _txt
                           <$> V.headM notWsEnts))

    -- WANT: Proof that e supplied to makeLine has non-empty text.
    spill (Pair toks (e :| es)) =
      if V.length toks > 0 || V.length e > 0 then
        let (theLine, newToks, rest) = makeLine toks e
        in Just (theLine, Pair newToks (rest :| es))
      else case es of
        [] -> Nothing
        x : xs -> spill (Pair V.empty (x :| xs))

    -- WANT: Proof that input to tokenize is an ent with non-empty text.
    makeLine tks ents =
      let (use_toks, new_ents) = if V.length tks > 0 then (tks, ents)
                                 else (tokenize (V.head ents), V.tail ents)
          (took, rest) = makeLineFromToks indent settings limit use_toks
      in (took, rest, new_ents)

data SplitWord = Split | Whole
  deriving Eq

makeLineFromToks
  :: Int
  -> WrapSettings
  -> Int
  -> Vector Token
  -> (Vector Entry, Vector Token)
makeLineFromToks indent settings limit toks =
  let remWidth = if preserveIndentation settings then limit - indent else limit
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
      (lhs, rhs) =
        assert (end_i > 0)
        $ if splitFirstWord == Split
          then undefined
          else undefined
  in undefined

{-
-- | Break a token sequence so that all tokens up to but not exceeding
-- a length limit are included on the left, and if any remain on the
-- right, return Just those too (or Nothing if there weren't any). If
-- this breaks a sequence at at point where the next token after the
-- break point is whitespace, that whitespace token is removed.
breakTokens :: WrapSettings -> Int -> [Token] -> ([Token], Maybe [Token])
breakTokens _ _ [] = ([], Nothing)
breakTokens settings limit ts =
    -- Take enough tokens until we reach the point where taking more
    -- would exceed the line length.
    let go _ []     = ([], [])
        -- Check to see whether the next token exceeds the limit. If so, bump
        -- it to the next line and terminate. Otherwise keep it and continue to
        -- the next token.
        go acc (tok:toks) =
            if tokenLength tok + acc <= limit
            then let (nextAllowed, nextDisallowed) = go (acc + tokenLength tok) toks
                 in (tok : nextAllowed, nextDisallowed)
            else case tok of
                     WS _ -> ([], toks)
                     NonWS _ ->
                         if acc == 0 && breakLongWords settings
                         then let (h, tl) = T.splitAt limit (tokenContent tok)
                              in ([NonWS h], NonWS tl : toks)
                         else if acc == 0 then ([tok], toks)
                         else ([], tok:toks)

        -- Allowed tokens are the ones we keep on this line. The rest go
        -- on the next line, to be wrapped again.
        (allowed, disallowed') = go 0 ts
        disallowed = maybeTrim disallowed'

        -- Trim leading whitespace on wrapped lines.
        maybeTrim [] = []
        maybeTrim (WS _:toks) = toks
        maybeTrim toks = toks

        result = if null disallowed
                 then (allowed, Nothing)
                 else (allowed, Just disallowed)
    in result
-}

data Token = Token { isWS :: !Bool, entry :: !Entry }

-- Postcondition: Entry inside a Token will be non-empty.
-- If input is non-empty, output vector will be non-empty.
tokenize :: Entry -> Vector Token
tokenize = V.unfoldr $ \Entry{_txt = t, width, attrIx} ->
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
           (e1, e2) = (Entry{_txt = pre, width = pre_w, attrIx},
                       Entry{_txt = post, width = post_w, attrIx})
       in Just (Token{isWS, entry = e1}, e2)
