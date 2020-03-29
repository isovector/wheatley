{-# OPTIONS_GHC -Wall #-}

module Typecheck where

import AST
import Control.Monad.State
import qualified Data.Map as M

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
runTc = flip evalState (TcState 0 mempty mempty) . runTc'

mkMeta :: Type -> TcM TcMeta
mkMeta ty = do
  scope <- fold <$> gets tcsScope
  pure $ TcMeta ty scope

getTypeOf :: Name -> TcM Type
getTypeOf = undefined


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



typeOf :: LExpr TcMeta -> (Type, LExpr TcMeta)
typeOf e@(EL _ tcm _) = (tcmType tcm, e)

unify :: Type -> Type -> TcM ()
unify = undefined



wiredIn_int :: Type
wiredIn_int = ConT $ Name "Int"


fresh :: TcM Type
fresh = undefined


withScope :: Map Name Type -> TcM a -> TcM a
withScope scope (TcM m) =
  TcM $ withState (\tcs -> tcs { tcsScope = scope : tcsScope tcs }) m

