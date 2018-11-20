{-# LANGUAGE DeriveAnyClass #-}

module Help.Ozil.KeyBinding
  ( Action (..)
  , KeyBinding
  , mkBinding
  , parseKeyBinding
  , ParseError (..)
  )
  where

import Data.Aeson
import Text.Megaparsec

import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import Data.List (nub, intercalate)
import Data.String (IsString)
import GHC.Generics (Generic)
import Graphics.Vty.Input (Key (..), Modifier (..))

import qualified Data.Text as T
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- * Actions

data Action
  = ScrollUp
  | ScrollDown
  | ScrollUpHalfPage
  | ScrollDownHalfPage
  | LinkFollow
  | LinkGoBack
  | LinkJumpNext
  | LinkJumpPrevious
  | ExitProgram
  deriving (Eq, Generic, Hashable)

instance Show Action where
  show = \case
    ScrollUp -> "scroll-up"
    ScrollDown -> "scroll-down"
    ScrollUpHalfPage -> "scroll-up-half-page"
    ScrollDownHalfPage -> "scroll-down-half-page"
    LinkFollow -> "link-follow"
    LinkGoBack -> "link-go-back"
    LinkJumpNext -> "link-jump-next"
    LinkJumpPrevious -> "link-jump-previous"
    ExitProgram -> "exit-program"

instance ToJSON Action where
  toJSON = String . T.pack . show

instance ToJSONKey Action where

instance FromJSON Action where
  parseJSON (String s) = case s of
    "scroll-up" -> pure ScrollUp
    "scroll-down" -> pure ScrollDown
    "scroll-up-half-page" -> pure ScrollUpHalfPage
    "scroll-down-half-page" -> pure ScrollDownHalfPage
    "link-follow" -> pure LinkFollow
    "link-go-back" -> pure LinkGoBack
    "link-jump-next" -> pure LinkJumpNext
    "link-jump-previous" -> pure LinkJumpPrevious
    "exit-program" -> pure ExitProgram
    _ -> fail "Unexpected action name."
  parseJSON invalid = typeMismatch "Action" invalid

instance FromJSONKey Action where

--------------------------------------------------------------------------------
-- * Keys

-- Invariants: The list has unique elements.
data KeyBinding = KeyBinding !Key [Modifier]

mkBinding :: Key -> [Modifier] -> KeyBinding
mkBinding k mods = KeyBinding k (nub mods)

instance Show KeyBinding where
  show (KeyBinding k mods) = intercalate "-" (map display mods ++ [showKey k])
    where
      display = \case
        MMeta  -> "meta"
        MAlt   -> "alt"
        MCtrl  -> "ctrl"
        MShift -> "shift"
      showKey = \case
        KEsc -> "esc"
        KChar c -> [c]
        KBS -> "back"
        KEnter -> "enter"
        KLeft -> "left"
        KRight -> "right"
        KUp -> "up"
        KDown -> "down"
        KUpLeft -> "upleft"
        KUpRight -> "upright"
        KDownLeft -> "downleft"
        KDownRight -> "downright"
        KCenter -> "center"
        KFun i -> "fn" <> show i
        KBackTab -> "backtab"
        KPrtScr -> "prtsc"
        KPause -> "pause"
        KIns -> "insert"
        KHome -> "home"
        KPageUp -> "pageup"
        KDel -> "delete"
        KEnd -> "end"
        KPageDown -> "pagedown"
        KBegin -> "begin" -- this is a key?
        KMenu -> "menu"

instance ToJSON KeyBinding where
  toJSON = String . T.pack . show

instance FromJSON KeyBinding where
  parseJSON (String s) = case parseKeyBinding s of
    Left e  -> fail (show e)
    Right x -> pure x
  parseJSON invalid = typeMismatch "KeyBinding" invalid

parseKeyBinding
  :: (Stream s, Token s ~ Char, IsString (Tokens s))
  => s
  -> Either (ParseError Char ()) KeyBinding
parseKeyBinding = parse keyBindingP ""

(-->) :: MonadParsec e s m => Tokens s -> a -> m a
(-->) s v = C.string s >> pure v

modifierP :: (MonadParsec e s m, IsString (Tokens s)) => m Modifier
modifierP =
  "meta" --> MMeta
  <|> "alt" --> MAlt
  <|> "ctrl" --> MCtrl
  <|> "shift" --> MShift

keyP :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m Key
keyP =
  "esc" --> KEsc
  <|> "back" --> KBS
  <|> "enter" --> KEnter
  <|> "left" --> KLeft
  <|> "right" --> KRight
  <|> "up" --> KUp
  <|> "down" --> KDown
  <|> "upleft" --> KUpLeft
  <|> "upright" --> KUpRight
  <|> "downleft" --> KDownLeft
  <|> "downright" --> KDownRight
  <|> "center" --> KCenter
  <|> "backtab" --> KBackTab
  <|> "prtsc" --> KPrtScr
  <|> "pause" --> KPause
  <|> "insert" --> KIns
  <|> "home" --> KHome
  <|> "pageup" --> KPageUp
  <|> "delete" --> KDel
  <|> "end" --> KEnd
  <|> "pagedown" --> KPageDown
  <|> "begin" --> KBegin
  <|> "menu" --> KMenu
  <|> KFun <$> (C.string "fn" *> L.decimal)
  <|> KChar <$> C.anyChar

keyBindingP
  :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s))
  => m KeyBinding
keyBindingP = do
  mods <- modifierP `endBy` C.char '-'
  k <- keyP
  pure (KeyBinding k (nub mods))
