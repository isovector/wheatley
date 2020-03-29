{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall         #-}

module Typecheck where

import AST
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Schemes
import Data.Generics.Aliases

data TcState = TcState
  { tcsFresh :: Int
  , tcsSubst :: Map Name Type
  , tcsScope :: [Map Name Type]
  }
  deriving stock (Eq, Ord, Show)

newtype TcM a = TcM
  { runTc' :: State TcState a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState TcState)

runTc :: TcM a -> a
runTc = flip evalState (TcState 0 mempty [wiredIn_prelude]) . runTc'

mkMeta :: Type -> TcM TcMeta
mkMeta ty = do
  scope <- fold <$> gets tcsScope
  pure $ TcMeta ty scope

getTypeOf :: Name -> TcM Type
getTypeOf n = do
  scope <- fold <$> gets tcsScope
  pure $ fromMaybe (error "yo") $ M.lookup n scope


typecheckM :: LExpr () -> TcM (LExpr TcMeta)
typecheckM (EL ss _ (LitE (IntLit n))) = do
  meta <- mkMeta wiredIn_int
  pure $ EL ss meta $ LitE $ IntLit n
typecheckM (EL ss _ (AppE f a)) = do
  ty <- fresh
  meta <- mkMeta ty
  (ft, fv) <- typeOf <$> typecheckM f
  (at, av) <- typeOf <$> typecheckM a
  unify ft $ at :-> ty
  pure $ EL ss meta $ AppE fv av
typecheckM (EL ss _ (VarE n)) = do
  ty <- getTypeOf n
  meta <- mkMeta ty
  pure $ EL ss meta (VarE n)
typecheckM (EL ss _ (LamE bndrs expr)) = do
  bndrts <- traverse (const fresh) bndrs
  (et, ev) <- withScope (M.fromList $ zip bndrs bndrts)
            $ typeOf <$> typecheckM expr
  tv <- fresh
  meta <- mkMeta tv
  unify tv $ foldr (:->) et bndrts
  pure $ EL ss meta $ LamE bndrs ev


zonk :: Data a => a -> TcM a
zonk a = do
  subst <- gets tcsSubst
  pure $ everywhere (mkT $ sub @Type subst) a





typeOf :: LExpr TcMeta -> (Type, LExpr TcMeta)
typeOf e@(EL _ tcm _) = (tcmType tcm, e)

unify :: Type -> Type -> TcM ()
unify t1 t2 = do
  s  <- gets tcsSubst
  s' <- mgu (sub s t1) (sub s t2)
  modify $ \z -> z { tcsSubst = tcsSubst z <> s' }
  pure ()


wiredIn_int :: Type
wiredIn_int = ConT $ LocalName "Int"

wiredIn_prelude :: Map Name Type
wiredIn_prelude = M.fromList
  [ (LocalName "plus", wiredIn_int :-> wiredIn_int :-> wiredIn_int)
  ]


fresh :: TcM Type
fresh = do
  v <- gets tcsFresh
  pure (VarT $ SystemName v)
    <* modify (\z -> z { tcsFresh = tcsFresh z + 1 })


withScope :: Map Name Type -> TcM a -> TcM a
withScope scope (TcM m) =
  TcM $ withState (\tcs -> tcs { tcsScope = scope : tcsScope tcs }) m

type Subst = Map Name Type

mgu :: Type -> Type -> TcM Subst
mgu (l :-> r) (l' :-> r') = do
  s1 <- mgu l l'
  s2 <- mgu (sub s1 r) (sub s1 r')
  pure $ s1 <> s2
mgu (AppT l r) (AppT l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (sub s1 r) (sub s1 r')
  pure $ s1 <> s2
mgu (ConT a) (ConT b)
  | a == b  = pure mempty
mgu (VarT u) t  = varBind u t
mgu t (VarT u)  = varBind u t
mgu t1 t2       = error $
  mconcat
    [ "types don't unify: '"
    , show t1
    , "' vs '"
    , show t2
    , "'"
    ]

varBind :: Name -> Type -> TcM Subst
varBind u t
  | t == VarT u = pure mempty
  | S.member u (free t) = error
      $ mconcat
        [ "occurs check: '"
        , show u
        , "' vs '"
        , show t
        , "'"
        ]
  | otherwise = do
      -- k <- kind t
      -- when (k /= tKind u) $ throwE "kind unification fails"
      pure $ M.singleton u t



class Types a where
  free :: a -> S.Set Name
  sub :: Subst -> a -> a

instance Types Type where
  free (VarT a)   = S.fromList [a]
  free (ConT _)   = S.fromList []
  free (AppT a b) = free a <> free b
  free (a :-> b)  = free a <> free b

  sub s (VarT n)   = maybe (VarT n) id $ M.lookup n s
  sub _ (ConT n)   = ConT n
  sub s (AppT a b) = sub s a `AppT` sub s b
  sub s (a :-> b)  = sub s a :-> sub s b



test :: LExpr ()
test = EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 23}), elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 22}), elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 2}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elMeta = (), elVal = LamE [LocalName "a",LocalName "b"] (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 17}), elMeta = (), elVal = AppE (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 10}) (SrcPos {srcPosLine = 1, srcPosCol = 15}), elMeta = (), elVal = VarE (LocalName "plus")}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 15}) (SrcPos {srcPosLine = 1, srcPosCol = 17}), elMeta = (), elVal = VarE (LocalName "a")})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 17}) (SrcPos {srcPosLine = 1, srcPosCol = 18}), elMeta = (), elVal = VarE (LocalName "b")})})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 20}) (SrcPos {srcPosLine = 1, srcPosCol = 22}), elMeta = (), elVal = LitE (IntLit 5)})}) (EL {elSpan = SrcSpan "yo" (SrcPos {srcPosLine = 1, srcPosCol = 22}) (SrcPos {srcPosLine = 1, srcPosCol = 23}), elMeta = (), elVal = LitE (IntLit 5)})}

