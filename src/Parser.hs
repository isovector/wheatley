module Parser where

import ParserUtils
import AST
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (some, many)


parseExpr :: Parser (LExpr ())
parseExpr = do
  exprs <- some parseExprNoApp
  pure $ foldl1' mkLAppE exprs

mkLAppE :: LExpr () -> LExpr () -> LExpr ()
mkLAppE f a = EL (envelope (elSpan f) (elSpan a)) () $ AppE f a

envelope :: SrcSpan -> SrcSpan -> SrcSpan
envelope (SrcSpan fp start _) (SrcSpan _ _ end)
  = SrcSpan fp start end


parseExprNoApp :: Parser (LExpr ())
parseExprNoApp =
  asum $ fmap try
    [ parens parseExpr
    , liftELoc parseLam
    , liftELoc parseLit
    , liftELoc parseVar
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

liftELoc :: Parser (e ()) -> Parser (ELoc e ())
liftELoc parser = do
  start <- getSourcePos
  p <- parser
  end <- getSourcePos
  pure $ EL (mkSrcSpan start end) () p

mkSrcSpan :: SourcePos -> SourcePos -> SrcSpan
mkSrcSpan (SourcePos fp line1 col1)
          (SourcePos _ line2 col2) =
  SrcSpan fp (SrcPos (unPos line1) (unPos col1))
             (SrcPos (unPos line2) (unPos col2))

parseLam :: Parser (Expr ())
parseLam = do
  symbol "\\"
  bndrs <- sepBy (liftELoc parseBndr) sc
  arr
  body <- parseExpr
  pure $ LamE bndrs body

parseLit :: Parser (Expr ())
parseLit = do
  d <- lex L.decimal
  pure $ LitE $ IntLit d


arr :: Parser ()
arr = symbol "->"

startChar :: Parser Char
startChar = satisfy $ \c -> (isAlpha c && isLower c) || c == '_'

idChar :: Parser Char
idChar = satisfy $ \c -> isAlphaNum c || c == '_'

parseName :: Parser Name
parseName = label "identifier" $
  fmap (Name . pack) $ lex $ (:) <$> startChar <*> many idChar

parseBndr :: Parser (Bndr ())
parseBndr = fmap Bndr parseName

parseVar :: Parser (Expr ())
parseVar = fmap VarE $ liftELoc parseBndr


