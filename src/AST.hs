module AST where

data SrcPos = SrcPos
  { srcPosLine :: Int
  , srcPosCol  :: Int
  } deriving stock (Eq, Ord, Show)

data SrcSpan = SrcSpan FilePath SrcPos SrcPos
  deriving stock (Eq, Ord, Show)

data ELoc e a = EL
  { elSpan :: SrcSpan
  , elMeta :: a
  , elVal  :: e a
  }
  deriving stock (Functor, Foldable, Traversable)

instance Show (e a) => Show (ELoc e a) where
  showsPrec n = showsPrec n . elVal


deriving instance (Eq (e a), Eq a) => Eq (ELoc e a)
deriving instance (Ord (e a), Ord a) => Ord (ELoc e a)


data Loc e = L SrcSpan e
  deriving stock (Eq, Ord, Show)


type LExpr = ELoc Expr
data Expr a
  = LitE Literal
  | AppE (LExpr a) (LExpr a)
  | LamE [LBndr a] (LExpr a)
  | VarE (LBndr a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Literal = IntLit Int
  deriving stock (Eq, Ord, Show)

type LBndr = ELoc Bndr
data Bndr a = Bndr Name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Name = Name Text
  deriving stock (Eq, Ord, Show)

