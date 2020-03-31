module Parser where

import ParserUtils
import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (some, many)
import qualified Data.Text.IO as T


parseExpr :: Parser (LExpr ())
parseExpr = do
  exprs <- some parseExprNoApp
  pure $ foldl1' mkLAppE exprs
  where
    mkLAppE :: LExpr () -> LExpr () -> LExpr ()
    mkLAppE f a = EL (envelope (elSpan f) (elSpan a)) False () $ AppE f a

    parseExprNoApp :: Parser (LExpr ())
    parseExprNoApp =
      asum $ fmap try
        [ parens parseExpr
        , liftELoc parseLam
        , liftELoc parseLit
        , liftELoc parseVar
        ]

    envelope :: SrcSpan -> SrcSpan -> SrcSpan
    envelope (SrcSpan fp start _) (SrcSpan _ _ end)
      = SrcSpan fp start end


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

liftELoc :: Parser (e ()) -> Parser (ELoc e ())
liftELoc parser = do
  start <- getSourcePos
  p <- parser
  end <- getSourcePos
  pure $ EL (mkSrcSpan start end) False () p

mkSrcSpan :: SourcePos -> SourcePos -> SrcSpan
mkSrcSpan (SourcePos fp line1 col1)
          (SourcePos _ line2 col2) =
  SrcSpan fp (SrcPos (unPos line1) (unPos col1))
             (SrcPos (unPos line2) (unPos col2))

parseLam :: Parser (Expr ())
parseLam = do
  symbol "\\"
  bndrs <- sepBy parseIdName sc
  arr
  body <- parseExpr
  pure $ LamE bndrs body

parseLit :: Parser (Expr ())
parseLit = do
  d <- lex L.decimal
  pure $ LitE $ IntLit d


arr :: Parser ()
arr = symbol "->"

sig :: Parser ()
sig = symbol ":"

startIdChar :: Parser Char
startIdChar = satisfy $ \c -> (isAlpha c && isLower c) || c == '_'

startTyChar :: Parser Char
startTyChar = satisfy $ \c -> isAlpha c && isUpper c

idChar :: Parser Char
idChar = satisfy $ \c -> isAlphaNum c || c == '_'

parseIdName :: Parser Name
parseIdName = label "identifier" $
  fmap (LocalName . pack) $ lex $ (:) <$> startIdChar <*> many idChar

parseVar :: Parser (Expr ())
parseVar = fmap VarE parseIdName

parseTyName :: Parser Name
parseTyName = label "constructor" $
  fmap (LocalName . pack) $ lex $ (:) <$> startTyChar <*> many idChar


parseType :: Parser Type
parseType = asum
  [ try parseForall
  , fmap (foldr1 (:->)) $ parseAppType `sepBy1` arr
  ]
  where
    parseAppType :: Parser Type
    parseAppType = do
      exprs <- some parseSimpleType
      pure $ foldl1' AppT exprs

    parseSimpleType :: Parser Type
    parseSimpleType = asum
      [ try $ parens parseType
      , try $ ConT <$> parseTyName
      , try $ VarT <$> parseIdName
      ]

    parseForall :: Parser Type
    parseForall = do
      symbol "forall"
      ns <- some parseIdName
      symbol "."
      t <- parseType
      pure $ ForallT ns t


parsePat :: Parser Pat
parsePat = asum
  [ try $ symbol "_" >> pure WildP
  , try $ VarP <$> parseIdName
  ]


parseSig :: Parser (Dec ())
parseSig = do
  n <- parseIdName
  sig
  t <- parseType
  endl
  pure $ SigD n t


parseFun :: Parser (Dec ())
parseFun = do
  n <- parseIdName
  pats <- many parsePat
  symbol "="
  body <- L.lineFold scn $ flip withSpace parseExpr
  endl
  pure $ FunD n pats body


liftLoc :: Parser (e ()) -> Parser (Loc e ())
liftLoc parser = do
  start <- getSourcePos
  p <- parser
  end <- getSourcePos
  pure $ L (mkSrcSpan start end) False p

parseDec :: Parser (LDec ())
parseDec = nonIndented $ liftLoc $ asum
  [ try parseSig
  , try parseFun
  ]


endl :: Parser ()
endl = void eol <|> eof


main :: IO ()
main = do
  let file = "code/test.wheat"
  fc <- T.readFile file
  putStrLn $ either errorBundlePretty show $ parse (some parseDec) file fc

test :: LExpr ()
test = EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 23}), elDirty = False, elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 22}), elDirty = False, elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elDirty = False, elMeta = (), elVal = LamE [LocalName "a",LocalName "b"] (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elDirty = False, elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 17}), elDirty = False, elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 15}), elDirty = False, elMeta = (), elVal = VarE (LocalName "plus")}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 15}) (SrcPos {srcPosLine = 1, srcPosCol = 17}), elDirty = False, elMeta = (), elVal = VarE (LocalName "a")})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 17}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elDirty = False, elMeta = (), elVal = VarE (LocalName "b")})})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 20}) (SrcPos {srcPosLine = 1, srcPosCol = 22}), elDirty = False, elMeta = (), elVal = LitE (IntLit 5)})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 22}) (SrcPos {srcPosLine = 1, srcPosCol = 23}), elDirty = False, elMeta = (), elVal = LitE (IntLit 5)})}
