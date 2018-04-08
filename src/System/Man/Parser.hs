module System.Man.Parser where

import Data.Either
import Data.Maybe
import Data.Void
import System.Man.Types
import Text.Megaparsec

import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

spaceP :: Parser ()
spaceP = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceP

-- Apparently, file names are a mess on Linux (surprise, surprise).
-- https://www.dwheeler.com/essays/fixing-unix-linux-filenames.html
--
-- For simplicity, we just assume that the characters are from the
-- Portable Filename Character set
-- http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_276
-- A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
-- a b c d e f g h i j k l m n o p q r s t u v w x y z
-- 0 1 2 3 4 5 6 7 8 9 . _ -
filepathP :: Parser FilePath
filepathP = lexeme (someTill isPathChar C.spaceChar)
 where
  isPathChar =
    C.satisfy (\c -> c == '/' || isPortableFilenameChar c)
      <?> "Encountered a character outside the Portable Filename Character Set."
  isPortableFilenameChar c =
    (c == '-' || c == '_' || c == '.')
      || ('0' <= c && c <= '9')
      || ('A' <= c && c <= 'Z')
      || ('a' <= c && c <= 'z')

mandatoryManpathP :: Parser FilePath
mandatoryManpathP = lexeme (C.string "MANDATORY_MANPATH") >> filepathP

manpathMapP :: Parser (FilePath, FilePath)
manpathMapP =
  lexeme (C.string "MANPATH_MAP") >> ((,) <$> filepathP <*> filepathP)

-- In general, /etc/manpath.config has more information
manpathConfigP :: Parser ManpathConfig
manpathConfigP = do
  (mandatory, maps) <- accumP
  pure $ ManpathConfig (V.fromList mandatory) (Map.fromList maps)
  where
    oneLineP :: Parser (Maybe (Either FilePath (FilePath, FilePath)))
    oneLineP = (Just <$>
      ((Left <$> mandatoryManpathP) <|> (Right <$> manpathMapP))
      )
      <|> (Nothing <$ skipManyTill C.anyChar (C.char '\n'))
    accumP = partitionEithers . catMaybes <$> many oneLineP

parseManpathConfig :: T.Text -> ManpathConfig
parseManpathConfig = undefined
