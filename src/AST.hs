module AST where

data SrcPos = SrcPos
  { srcPosLine :: Int
  , srcPosCol  :: Int
  } deriving stock (Eq, Ord, Show, Data, Generic)

data SrcSpan = SrcSpan FilePath SrcPos SrcPos
  deriving stock (Eq, Ord, Show, Data, Generic)

data ELoc e a = EL
  { elSpan  :: SrcSpan
  , elDirty :: Bool
  , elMeta  :: a
  , elVal   :: e a
  }
  deriving stock (Functor, Foldable, Traversable, Data, Generic)

data Loc e a = L
  { lSpan  :: SrcSpan
  , lDirty :: Bool
  , lVal   :: e a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Data, Generic)

data TcMeta = TcMeta
  { tcmType :: Type
  , tcmLocal :: LocalScope
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

-- instance Show (e a) => Show (ELoc e a) where
--   showsPrec n = showsPrec n . elVal


deriving instance (Show (e a), Show a) => Show (ELoc e a)
deriving instance (Eq (e a), Eq a) => Eq (ELoc e a)
deriving instance (Ord (e a), Ord a) => Ord (ELoc e a)


type LExpr = ELoc Expr
data Expr a
  = LitE Literal
  | AppE (LExpr a) (LExpr a)
  | LamE [Name] (LExpr a)
  | VarE Name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Data, Generic)

data Literal = IntLit Int
  deriving stock (Eq, Ord, Show, Data, Generic)

data Name
  = LocalName Text
  | QualifiedName Text Text Text
  | SystemName Int
  deriving stock (Eq, Ord, Show, Data, Generic)


data Type
  = ConT Name
  | VarT Name
  | AppT Type Type
  | Type :-> Type
  | ForallT [Name] Type
  deriving stock (Eq, Ord, Show, Data, Generic)

type LocalScope = Map Name Type

type LDec = Loc Dec
data Dec a
  = FunD Name [Pat] (LExpr a)
  | SigD Name Type
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Data, Generic)

data Pat
  = VarP Name
  | WildP
  deriving stock (Eq, Ord, Show, Data, Generic)




infixr 0 :->

