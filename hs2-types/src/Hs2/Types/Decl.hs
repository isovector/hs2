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

instance Bifunctor Clause where
  bimap = bimapDefault

instance Bifoldable Clause where
  bifoldMap = bifoldMapDefault

instance Bitraversable Clause where
  bitraverse = bitrav


data Decl name meta
  = Func name [Loc meta (Clause name meta)]
  | TypeSig name (DLoc meta (Type name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Decl where
  bimap = bimapDefault

instance Bifoldable Decl where
  bifoldMap = bifoldMapDefault

instance Bitraversable Decl where
  bitraverse = bitrav


data TopDecl name meta
  = TopDecl (DLoc meta (Decl name meta))
  | DataDecl
      name
      [DLoc meta (TyVar name meta)]
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor TopDecl where
  bimap = bimapDefault

instance Bifoldable TopDecl where
  bifoldMap = bifoldMapDefault

instance Bitraversable TopDecl where
  bitraverse = bitrav

