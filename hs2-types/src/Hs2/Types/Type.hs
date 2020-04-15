module Hs2.Types.Type where

type Cxt = Type

data PromotableType
  = ConTy Name
  | VarTy Name
  | TupleTy Int

data Type
  = ForallTy [Name] Type
  | ConstrainTy Cxt Type
  | AppTy Type Type
  | RegularTy PromotableType
  | PromotedTy PromotableType
  | LitTy TyLit
  | ArrowTy Type Type

data TyLit
  = NumTyLit Integer
  | StrTyLit String

data Name

