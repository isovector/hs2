module Hs2.Types.Type where

import Data.Bifunctor
import GHC.Generics
import Data.Data
import Hs2.Types.Loc
import Data.Bitraversable
import Data.Bifoldable

type Cxt name meta = Type name meta

data PromotableType name
  = ConTy name
  | VarTy name
  | TupleTy Int
  deriving stock (Eq, Ord, Show, Functor, Data, Generic, Foldable, Traversable)

data Type name meta
  = ForallTy
      [name]
      (DLoc meta (Type name meta))
  | ConstrainTy
      (Cxt name meta)
      (DLoc meta (Type name meta))
  | AppTy
      (DLoc meta (Type name meta))
      (DLoc meta (Type name meta))
  | RegularTy (PromotableType name)
  | PromotedTy (PromotableType name)
  | LitTy TyLit
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Type where
  bimap = bimapDefault

instance Bifoldable Type where
  bifoldMap = bifoldMapDefault

instance Bitraversable Type where
  bitraverse = bitrav

data TyLit
  = NumTyLit Integer
  | StrTyLit String
  deriving stock (Eq, Ord, Show, Data, Generic)

