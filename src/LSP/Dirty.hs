module LSP.Dirty
  ( getDirty
  ) where

import AST
import GHC.Generics
import qualified Data.Map as M
import Ppr


class GGetDirty f where
  ggetDirty :: f x -> Map SrcSpan Text

instance GGetDirty f => GGetDirty (M1 _1 _2 f) where
  ggetDirty = ggetDirty . unM1

instance (GGetDirty f, GGetDirty g) => GGetDirty (f :*: g) where
  ggetDirty (f :*: g) = ggetDirty f <> ggetDirty g

instance (GGetDirty f, GGetDirty g) => GGetDirty (f :+: g) where
  ggetDirty (L1 f) = ggetDirty f
  ggetDirty (R1 g) = ggetDirty g

instance GGetDirty U1 where
  ggetDirty U1 = mempty

instance (GGetDirty (Rep (e a)), Generic (e a), Ppr (e a))
      => GGetDirty (K1 _1 (ELoc e a)) where
  ggetDirty (K1 (EL src True _ v)) =
    M.singleton src $ pack $ render $ ppr v
  ggetDirty (K1 (EL _ False _ v)) =
    getDirty v

instance (GGetDirty (Rep (e a)), Generic (e a), Ppr (e a))
      => GGetDirty (K1 _1 (Loc e a)) where
  ggetDirty (K1 (L src True v)) =
    M.singleton src $ pack $ render $ ppr v
  ggetDirty (K1 (L _ False v)) =
    getDirty v

instance {-# OVERLAPPABLE #-} (GGetDirty (Rep x), Generic x)
      => GGetDirty (K1 _1 x) where
  ggetDirty = getDirty .unK1

instance GGetDirty (K1 _1 Char) where
  ggetDirty _ = mempty

instance GGetDirty (K1 _1 Int) where
  ggetDirty _ = mempty

instance GGetDirty (K1 _1 Text) where
  ggetDirty _ = mempty

getDirty :: (GGetDirty (Rep x), Generic x) => x -> Map SrcSpan Text
getDirty = ggetDirty . from


