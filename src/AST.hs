module AST where

data SrcPos = SrcPos
  { srcPosLine :: Int
  , srcPosCol  :: Int
  } deriving stock (Eq, Ord, Show, Data)

data SrcSpan = SrcSpan FilePath SrcPos SrcPos
  deriving stock (Eq, Ord, Show, Data)

data ELoc e a = EL
  { elSpan :: SrcSpan
  , elMeta :: a
  , elVal  :: e a
  }
  deriving stock (Functor, Foldable, Traversable, Data)

data TcMeta = TcMeta
  { tcmType :: Type
  , tcmLocal :: LocalScope
  }
  deriving stock (Eq, Ord, Show, Data)

-- instance Show (e a) => Show (ELoc e a) where
--   showsPrec n = showsPrec n . elVal


deriving instance (Show (e a), Show a) => Show (ELoc e a)
deriving instance (Eq (e a), Eq a) => Eq (ELoc e a)
deriving instance (Ord (e a), Ord a) => Ord (ELoc e a)


data Loc e = L SrcSpan e
  deriving stock (Eq, Ord, Show, Data)


type LExpr = ELoc Expr
data Expr a
  = LitE Literal
  | AppE (LExpr a) (LExpr a)
  | LamE [Name] (LExpr a)
  | VarE Name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Data)

data Literal = IntLit Int
  deriving stock (Eq, Ord, Show, Data)

data Name
  = LocalName Text
  | QualifiedName Text Text Text
  | SystemName Int
  deriving stock (Eq, Ord, Show, Data)


data Type
  = ConT Name
  | VarT Name
  | AppT Type Type
  | Type :-> Type
  deriving stock (Eq, Ord, Show, Data)

type LocalScope = Map Name Type

infixr 0 :->

