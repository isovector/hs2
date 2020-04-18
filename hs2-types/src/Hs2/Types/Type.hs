module Hs2.Types.Type where

import GHC.Generics
import Data.Data

type Cxt name = Type name

data PromotableType name
  = ConTy name
  | VarTy name
  | TupleTy Int
  deriving stock (Eq, Ord, Show, Functor, Data, Generic, Foldable, Traversable)

data Type name
  = ForallTy [name] (Type name)
  | ConstrainTy (Cxt name) (Type name)
  | AppTy (Type name) (Type name)
  | RegularTy (PromotableType name)
  | PromotedTy (PromotableType name)
  | LitTy TyLit
  deriving stock (Eq, Ord, Show, Functor, Data, Generic, Foldable, Traversable)

data TyLit
  = NumTyLit Integer
  | StrTyLit String
  deriving stock (Eq, Ord, Show, Data, Generic)

