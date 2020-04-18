module Hs2.Types.Expr where

import Hs2.Types.Pat
import Hs2.Types.Loc
import Hs2.Types.Type
import GHC.Generics
import Data.Data
import Data.Bifunctor


type MatchGroup name meta a = [Match name meta]

data Expr name meta
  = Var name
  | Con name
  | UnboundVar name
  | Lit Lit
  | ListLit [Loc meta (Expr name meta)]
  | TupleLit [Loc meta (Expr name meta)]
  | Lam
      [(Pat name meta)]
      (Loc meta (Expr name meta))
  | App
      (Loc meta (Expr name meta))
      (Loc meta (Expr name meta))
  | TyApp
      (Loc meta (Expr name meta))
      (Type name)
  | Case
      (Loc meta (Expr name meta))
      (MatchGroup name meta (Loc meta (Expr name meta)))
  | Let
      [LocalBind name meta]
      (Loc meta (Expr name meta))
  | Do [Stmt name meta]
  | RecordNew
      name
      [FieldValue name meta]
  | RecordUpdate
      (Loc meta (Expr name meta))
      [FieldValue name meta]
  | TySig
      (Loc meta (Expr name meta))
      (Type name)
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Expr where
  bimap = glocmap

data Stmt name meta
  = Bind (Loc meta (Pat name meta)) (Loc meta (Expr name meta))
  | LetStmt [LocalBind name meta]
  | NoBind (Loc meta (Expr name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Stmt where
  bimap = glocmap

data Lit
  = Char Char
  | String String
  | Integer Integer
  | Rational Rational
  deriving stock (Eq, Ord, Show, Data, Generic)

data Match name meta
  = Match
      (Loc meta (Pat name meta))
      (Loc meta (Expr name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor Match where
  bimap = glocmap

data FieldValue name meta
  = FieldValue
      name
      (Loc meta (Expr name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor FieldValue where
  bimap = glocmap

data LocalBind name meta
  = LocalBind
      (Pat name meta)
      (Loc meta (Expr name meta))
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Bifunctor LocalBind where
  bimap = glocmap

