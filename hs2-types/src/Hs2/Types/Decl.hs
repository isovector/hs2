module Hs2.Types.Decl where

import GHC.Generics
import Data.Data
import Hs2.Types.Loc
import Hs2.Types.Type
import Hs2.Types.Expr
import Hs2.Types.Pat
import Data.Bifunctor
import Data.Bitraversable
import Data.Bifoldable

data Clause name meta
  = Clause
      [Loc meta (Pat name meta)]
      (Loc meta (Expr name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor     Clause where bimap = bimapDefault
instance Bifoldable    Clause where bifoldMap = bifoldMapDefault
instance Bitraversable Clause where bitraverse = bitrav


data Decl name meta
  = Func name [Loc meta (Clause name meta)]
  | TypeSig name (DLoc meta (Type name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor     Decl where bimap = bimapDefault
instance Bifoldable    Decl where bifoldMap = bifoldMapDefault
instance Bitraversable Decl where bitraverse = bitrav


data TopDecl name meta
  = TopDecl (DLoc meta (Decl name meta))
  | DataDecl
      name
      [DLoc meta (TyVar name meta)]
      [DLoc meta (Con name meta)]
  | NewtypeDecl
      name
      (DLoc meta (TyVar name meta))
  | TySynDecl
      name
      [DLoc meta (TyVar name meta)]
      (DLoc meta (Type name meta))
  | ClassDecl
      (DLoc meta (Cxt name meta))
      name
      [DLoc meta (TyVar name meta)]
      [DLoc meta (Decl name meta)]
  | InstanceDecl
      (DLoc meta (Cxt name meta))
      (DLoc meta (Type name meta))
      [DLoc meta (Decl name meta)]
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor     TopDecl where bimap = bimapDefault
instance Bifoldable    TopDecl where bifoldMap = bifoldMapDefault
instance Bitraversable TopDecl where bitraverse = bitrav


data Con name meta
  = NormalCon
      name
      [DLoc meta (Type name meta)]
  | RecordCon
      name
      [DLoc meta (FieldDecl name meta)]
  | GadtCon
      name
      [DLoc meta (Type name meta)]
      (DLoc meta (Type name meta))
  | RecordGadtCon
      name
      [DLoc meta (FieldDecl name meta)]
      (DLoc meta (Type name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor     Con where bimap = bimapDefault
instance Bifoldable    Con where bifoldMap = bifoldMapDefault
instance Bitraversable Con where bitraverse = bitrav


data FieldDecl name meta
  = FieldDecl name (DLoc meta (Type name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor     FieldDecl where bimap = bimapDefault
instance Bifoldable    FieldDecl where bifoldMap = bifoldMapDefault
instance Bitraversable FieldDecl where bitraverse = bitrav

