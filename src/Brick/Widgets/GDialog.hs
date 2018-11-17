{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Brick.Widgets.GDialog
  ( ButtonLayout (..)
  , GDialog (..)
  , getDialog
  , setDialog
  , mapDialog
  , renderGDialog
  , Pos (..)
  , ButtonSelect (..)
  , simpleHandleEvent
  )
where

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, hBox, hLimit, withDefAttr, str, withAttr)
import Brick.Widgets.Center (hCenter, centerLayer)
import Brick.Widgets.Border (borderWithLabel, border)
import Data.List (intersperse)
import Graphics.Vty.Input (Event(..), Key(..))

import qualified Brick.Widgets.Dialog as D

data ButtonLayout = H | V

-- | Dialogs present a window with a title (optional), a body, and
-- buttons (optional). Dialog buttons are labeled with strings and map
-- to values of type 'a', which you choose.
--
-- Dialogs handle the following events by default with
-- handleDialogEvent:
--
-- * Tab or Right Arrow: select the next button
-- * Shift-tab or Left Arrow: select the previous button
data GDialog (d :: ButtonLayout) a where
  HDialog :: D.Dialog a -> GDialog 'H a
  VDialog :: D.Dialog a -> GDialog 'V a

getDialog :: GDialog d a -> D.Dialog a
getDialog = \case
  HDialog x -> x
  VDialog x -> x

setDialog :: D.Dialog a -> GDialog d a -> GDialog d a
setDialog d = \case
  HDialog _ -> HDialog d
  VDialog _ -> VDialog d

mapDialog :: (D.Dialog a -> D.Dialog b) -> GDialog d a -> GDialog d b
mapDialog f = \case
  HDialog d -> HDialog (f d)
  VDialog d -> VDialog (f d)

renderGDialog :: GDialog d a -> Widget n -> Widget n
renderGDialog d body =
  let di = getDialog d
      buttonPadding = str "  "
      mkButton (i, (s, _)) =
        let att = if Just i == D.dialogSelectedIndex di
                  then D.buttonSelectedAttr
                  else D.buttonAttr
        in withAttr att (str (" " <> s <> " "))
      buttons = boxfn (intersperse buttonPadding
                       $ mkButton <$> zip [0 ..] (D.dialogButtons di))
      doBorder = maybe border borderWithLabel (str <$> D.dialogTitle di)
  in
    centerLayer
    $ withDefAttr D.dialogAttr
    $ hLimit (D.dialogWidth di)
    $ doBorder
    $ vBox [body, hCenter buttons]
  where
    boxfn :: [Widget n] -> Widget n
    boxfn = case d of
      HDialog _ -> hBox
      VDialog _ -> vBox

data ButtonSelect = Next Int | Done | Unknown

data Pos = Pos { idx :: !Int, len :: !Int }

simpleHandleEvent :: Pos -> ButtonLayout -> Event -> ButtonSelect
simpleHandleEvent Pos{idx = i, len} lyt = \case
  EvKey (KChar '\t') [] -> nextButtonBy'   1 True
  EvKey KBackTab     [] -> nextButtonBy' (-1) True
  EvKey KEnter       [] -> Done

  EvKey KRight      [] | H <- lyt -> nextButtonBy'   1  False
  EvKey (KChar 'l') [] | H <- lyt -> nextButtonBy'   1  False

  EvKey KLeft       [] | H <- lyt -> nextButtonBy' (-1) False
  EvKey (KChar 'h') [] | H <- lyt -> nextButtonBy' (-1) False

  EvKey KDown       [] | V <- lyt -> nextButtonBy'   1  False
  EvKey (KChar 'j') [] | V <- lyt -> nextButtonBy' 1  False

  EvKey KUp         [] | V <- lyt -> nextButtonBy' (-1) False
  EvKey (KChar 'k') [] | V <- lyt -> nextButtonBy' (-1) False

  _ -> Unknown
  where
    inRange mn v mx = min mx (max mn v)
    nextButtonBy' di wrap = Next $
      if wrap then (i + di) `mod` len
      else inRange 0 (i + di) (len - 1)
