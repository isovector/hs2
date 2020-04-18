{-# LANGUAGE EmptyDataDeriving #-}

module Hs2.Types.Pat
  ( Pat (..)
  , FieldPat (..)
  ) where

import Data.Data
import Data.Bifunctor
import GHC.Generics
import Hs2.Types.Loc
import Hs2.Types.Type
import Data.Bitraversable
import Data.Bifoldable

data Pat name meta
  = WildPat
  | VarPat name
  | LazyPat (Loc meta (Pat name meta))
  | AsPat name (Loc meta (Pat name meta))
  | StrictPat (Loc meta (Pat name meta))
  | ConPat name [Loc meta (Pat name meta)]
  | LitPat Lit
  | TuplePat [Loc meta (Pat name meta)]
  | RecordPat name [FieldPat name meta]
  | SigPat (Loc meta (Pat name meta)) (Type name)
  -- | ViewPat Expr (Loc meta (Pat name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Pat where
  bimap = bimapDefault

instance Bifoldable Pat where
  bifoldMap = bifoldMapDefault

instance Bitraversable Pat where
  bitraverse = bitrav

data FieldPat name meta
  = FieldPat name (Loc meta (Pat name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor FieldPat where
  bimap = bimapDefault

instance Bifoldable FieldPat where
  bifoldMap = bifoldMapDefault

instance Bitraversable FieldPat where
  bitraverse = bitrav

data Lit
  deriving stock (Eq, Ord, Show, Data, Generic)

data Expr
  deriving stock (Eq, Ord, Show, Data, Generic)

