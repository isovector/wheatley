{-# LANGUAGE RankNTypes #-}

module LSP where

import Ppr
import AST
import Data.Generics.Aliases
import Data.Generics.Schemes
import Control.Monad.State

move :: Int -> Int -> SrcSpan -> SrcSpan
move dl dc (SrcSpan fp (SrcPos l0 c0) (SrcPos l1 c1)) =
  SrcSpan fp
    (SrcPos (l0 + dl) (c0 + dc))
    (SrcPos (l1 + dl) (c1 + dc))

resize :: Int -> Int -> SrcSpan -> SrcSpan
resize dl dc (SrcSpan fp (SrcPos l0 c0) (SrcPos l1 c1)) =
  SrcSpan fp
    (SrcPos l0 c0)
    (SrcPos (l1 + dl) (c1 + dc))

contains :: SrcSpan -> SrcSpan -> Bool
contains (SrcSpan afp (SrcPos al0 ac0) (SrcPos al1 ac1))
         (SrcSpan bfp (SrcPos bl0 bc0) (SrcPos bl1 bc1)) =
  and
    [ afp == bfp
    , al0 <= bl0
    , ac0 <= bc0
    , bl1 <= al1
    , bc1 <= ac1
    ]

srcSize :: SrcSpan -> (Int, Int)
srcSize (SrcSpan _ (SrcPos l0 c0) (SrcPos l1 c1))
  = (l1 - l0, c1 - c0)

replace
    :: forall e' x a
     . (Data (a x), Ppr (e' x), Typeable e', Typeable x)
    => SrcSpan
    -> e' x
    -> a x
    -> a x
replace src b = flip evalState (0, 0) . everywhereM (mkM $ \case
  (EL sp@(SrcSpan fp start@(SrcPos sl sc) _) dirty meta val :: ELoc e' x) -> do
    (l, c) <- get
    let f = bool move resize $ contains sp src
    case sp == src of
      True  -> do
        let (mesl, mesc) = measure b
            (spl, spc) = srcSize sp
        put (mesl - spl, mesc - spc)
        pure
          $ EL (SrcSpan fp start (SrcPos (sl + mesl) (sc + mesc))) True meta
          $ b
      False -> pure $ EL (f l c sp) dirty meta val
                                                            )

measure :: Ppr e => e -> (Int, Int)
measure e =
  let ls = lines $ render $ ppr e
   in (length ls - 1, length (last ls) + 1)

