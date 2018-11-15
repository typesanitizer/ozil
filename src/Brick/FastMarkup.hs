{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Brick.FastMarkup where

import Commons

import Brick (textWidth, Widget (..))
import Brick.Markup (GetAttr (..))
import Data.Functor.Identity (Identity (..))
import Data.Char (isSpace)
import Text.Wrap (defaultWrapSettings, WrapSettings (..))
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

data Entry (ne :: Emptiness) =
  Entry { _txt :: !Text, width :: !Int, attrIx :: !Int }
  deriving (Show, Generic, NFData)

data FastMarkup a = FastMarkup !(Vector Token) !(Vector a)

-- Postcondition: Entries have non-empty Text values.
mkFastMarkup :: Ord a => [(Text, a)] -> FastMarkup a
mkFastMarkup (filter (not . T.null . fst) -> tas) =
  let (_, as) = unzip tas
      as_list = Set.toList (Set.fromList as)
      as_map = Map.fromAscList (zip as_list [0 ..])
      tps = V.concatMap tokenize $ V.fromList
        [ Entry t (textWidth t) i | (t, a) <- tas, let i = as_map Map.! a ]
  in FastMarkup tps (V.fromList as_list)

fmWrap :: GetAttr a => FastMarkup a -> Widget n
fmWrap = fmWrapWith defaultWrapSettings

-- | FastMarkup analog of txtWrapWith
fmWrapWith :: HasCallStack => GetAttr a => WrapSettings -> FastMarkup a -> Widget n
fmWrapWith settings (FastMarkup tis as) =
  Widget Brick.Greedy Brick.Fixed $ do
    c <- Brick.getContext
    attrs <- traverse getAttr as
    let theLines = wrapLines settings (c ^. Brick.availWidthL) tis
        horizCat' = Vty.horizCat . V.toList
          . V.map (\(Entry t _ i :: Entry 'NE) ->
                     -- TFW your Haskell code looks like C code :(
                     let attr_i = if i == -1 then Vty.defAttr else attrs !!! i
                     in Vty.text' attr_i t
                  )
    pure $ case V.length theLines of
      0 -> Brick.emptyResult
      1 -> set Brick.imageL (horizCat' (theLines !!! 0)) Brick.emptyResult
      _ ->
        let lengths = V.map (V.sum . V.map width) theLines
            maxLen = V.maximum lengths
            lineImgs = V.imap lineImg theLines
            lineImg i line =
              let delta = maxLen - (lengths !!! i)
                  simpleImg = horizCat' line
                  padding = Vty.text' (c ^. Brick.attrL) (T.replicate delta " ")
              in if delta == 0 then simpleImg
                  else Vty.horizCat [simpleImg, padding]
        in set Brick.imageL (Vty.vertCat (V.toList lineImgs)) Brick.emptyResult

type Accum = Vector Token

wrapLines
  :: WrapSettings
  -> Int          -- ^ Wrapping width
  -> Vector Token
  -> Vector (Vector (Entry 'NE))
wrapLines settings limit vs = V.unfoldr spill vs
  where
    indent :: Int
    indent = if | V.length vs == 0 -> 0
                | otherwise ->
                  let x = vs V.! 0 in
                  if | isWS x    -> width (entry x)
                     | otherwise -> 0

    spill :: Accum -> Maybe (Vector (Entry 'NE), Accum)
    spill toks =
      if V.length toks > 0 && V.any (not . isWS) toks
      then Just (makeLineFromToks indent settings limit toks)
      else Nothing

data SplitWord = Split | Whole

padEntry :: Int -> Entry 'NE
padEntry w = assert (w > 0)
  $ Entry {_txt = T.replicate w " ", width = w, attrIx = -1} :: Entry 'NE

padLeft :: WrapSettings -> Int -> Vector (Entry 'NE) -> Vector (Entry 'NE)
padLeft settings indent =
  if preserveIndentation settings && indent > 0
  then V.cons (padEntry indent)
  else id

padRight :: Int -> Vector (Entry 'NE) -> Vector (Entry 'NE)
padRight i = assert (i >= 0) $ if i == 0 then id else flip V.snoc (padEntry i)

data Ixs = Ixs { usedWidth :: !Int, range :: !(Pair Int Int) }

{-# INLINE breakIxs #-}
breakIxs :: Vector Token -> WrapSettings -> Int -> Bool -> Ixs -> (SplitWord, Ixs)
breakIxs tokens settings availWidth = go
  where
    availLength = V.length tokens
    go trimPrefixWS ixs@Ixs{usedWidth, range = Pair start_i end_i} =
      if | end_i == availLength || usedWidth == availWidth -> (Whole, ixs)
         -- Now end_i < availLength
         -- Remove whitespace at the beginning of the line.
         | trimPrefixWS ->
           if | not (isWS (tokens !!! end_i)) -> go False ixs
              | otherwise -> go True ixs{range = Pair (start_i + 1) (end_i + 1)}
         -- trimPrefixWS is False now
         -- usedWidth < availWidth by induction
         | otherwise ->
           assert (usedWidth < availWidth)
           $ let width_i = width (entry (tokens !!! end_i))
                 usedWidth' = usedWidth + width_i
                 firstTokenVeryWide = end_i == 0 &&
                   assert (usedWidth == 0) (width_i > availWidth)
             in
             if | usedWidth' <= availWidth ->
                  go False (Ixs usedWidth' (Pair start_i (end_i + 1)))
                -- Now usedWidth' > availWidth
                | firstTokenVeryWide ->
                  ( if breakLongWords settings then Split else Whole
                  , Ixs availWidth (Pair 0 1)
                  )
                | otherwise -> (Whole, ixs)

{-# INLINE makeLineFromToks #-}
makeLineFromToks
  :: Int
  -> WrapSettings
  -> Int
  -> Vector Token
  -> (Vector (Entry 'NE), Vector Token)
makeLineFromToks indent settings limit toks = (lhs, rhs)
  where
    remWidth = if preserveIndentation settings then limit - indent else limit
    indentEntries = padLeft settings indent
    (splitFirstWord, Ixs{usedWidth, range = Pair start_i end_i}) =
      breakIxs toks settings remWidth True (Ixs 0 (Pair 0 0))
    (lhs, rhs) = assert (end_i > 0) $ case splitFirstWord of
      Whole ->
        let (takeToks, remToks) = assert (end_i > start_i)
              $ V.splitAt (end_i - start_i) (V.drop start_i toks)
            ents = V.map entry takeToks
        in (indentEntries $ padRight (remWidth - usedWidth) ents, remToks)
      Split ->
        let Entry{_txt,width,attrIx} = entry (toks !!! 0)
            (t1, rem_w, t2) = splitAtWidth usedWidth _txt
            e1 = Entry { _txt = t1 <> T.replicate rem_w " "
                       , width = usedWidth, attrIx } :: Entry 'NE
            e2 = assert (not (T.null t2))
              $ Entry { _txt = t2, width = width - usedWidth, attrIx }
            hdTok = Token { isWS = False, entry = e2 }
        in (indentEntries $ V.singleton e1, V.cons hdTok (V.tail toks))

-- If the supplied width is too large, the second text will be empty.
{-# INLINE splitAtWidth #-}
splitAtWidth :: Int -> Text -> (Text, Int, Text)
splitAtWidth max_w t = (fromVec v_pre, max_w - used_w, fromVec v_post)
  where
    fromVec = T.pack . V.toList
    v :: UVector Char
    v = V.fromList $ T.unpack t
    vlen = V.length v
    go w i = if i == vlen then (w, i)
             else let w_i = textWidth (Identity (v !!! i))
                  in if | w + w_i > max_w -> (w, i)
                        | otherwise -> go (w + w_i) (i + 1)
    (used_w, ix) = go 0 0
    (v_pre, v_post) = V.splitAt ix v

data Token = Token { isWS :: !Bool, entry :: !(Entry 'NE) }
  deriving Show

{-# INLINE tokenize #-}
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
