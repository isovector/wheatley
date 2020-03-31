{-# LANGUAGE ImplicitParams       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserUtils
  ( module ParserUtils
  , L.decimal
  ) where

import GHC.Classes
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (some, many)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space (void $ some $ C.char ' ')
          (L.skipLineComment "--")
          (L.skipBlockCommentNested "{-" "-}")

scn :: Parser ()
scn =
  L.space C.space1
          (L.skipLineComment "--")
          (L.skipBlockCommentNested "{-" "-}")

symbol :: (?sp :: Parser ()) => Text -> Parser ()
symbol = void . L.symbol ?sp

identGuard :: (?sp :: Parser ()) => Ordering -> Pos -> Parser Pos
identGuard = L.indentGuard ?sp

nonIndented :: (?sp :: Parser ()) => Parser a -> Parser a
nonIndented = L.nonIndented ?sp

lex :: (?sp :: Parser ()) => Parser a -> Parser a
lex = L.lexeme ?sp

-- https://kcsongor.github.io/global-implicit-parameters/
instance IP "sp" (Parser ()) where
  ip = sc

withSpace :: Parser () -> Parser a -> Parser a
withSpace sp p = let ?sp = sp in p

