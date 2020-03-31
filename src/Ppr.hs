{-# OPTIONS_GHC -Wall #-}

module Ppr
  ( module Ppr
  , render
  ) where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import AST

class Ppr a where
  ppr :: a -> Doc

instance Ppr (e a) => Ppr (ELoc e a) where
  ppr = ppr . elVal

instance Ppr (Expr a) where
  ppr (VarE n) = ppr n
  ppr (LitE (IntLit n)) = text $ show n
  ppr (AppE a b) = sep [ parens $ ppr a, parens $ ppr b ]
  ppr (LamE ns b) = sep
    [ hsep
        [ text "\\"
        , hsep $ fmap ppr ns
        , "->"
        ]
    , ppr b
    ]

instance Ppr Name where
  ppr (LocalName n) = text $ unpack n
  ppr (QualifiedName p m n) = mconcat $
    punctuate (text ".")
      [ text $ unpack p
      , text $ unpack m
      , text $ unpack n
      ]
  ppr (SystemName n) = text $ "__system_" <> show n
