module Hs2.Types.Expr where

import Hs2.Types.Pat
import Hs2.Types.Type


type MatchGroup a = [Match]

data Expr
  = Var Name
  | Con Name
  | UnboundVar Name
  | Lit Lit
  | ListLit [Expr]
  | TupleLit [Expr]
  | Lam [Pat] Expr
  | App Expr Expr
  | TyApp Expr Type
  | Case Expr (MatchGroup Expr)
  | Let [LocalBind] Expr
  | Do [Stmt]
  | RecordNew Name [FieldValue]
  | RecordUpdate Expr [FieldValue]
  | TySig Expr Type

data Stmt
  = Bind Pat Expr
  | LetStmt [LocalBind]
  | NoBind Expr

data Lit
  = Char Char
  | String String
  | Integer Integer
  | Rational Rational

data Match
  = Match Pat Expr

data FieldValue
  = FieldValue Name Expr

data LocalBind
  = LocalBind Pat Expr

