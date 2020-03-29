module ParserUtils
  ( module ParserUtils
  , module C
  , L.decimal
  ) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space C.space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

identGuard :: Ordering -> Pos -> Parser Pos
identGuard = L.indentGuard sc

lex :: Parser a -> Parser a
lex = L.lexeme sc

