module Hs2.Types.Pat
  ( Pat (..)
  ) where

import Hs2.Types.Type

data Pat
  = WildPat
  | VarPat Name
  | LazyPat Pat
  | AsPat Name Pat
  | StrictPat Pat
  | ConPat Name [Pat]
  | LitPat Lit
  | TuplePat [Pat]
  | RecordPat Name [FieldPat]
  | SigPat Pat Type
  | ViewPat Expr Pat

data FieldPat
  = FieldPat Name Pat

data Lit
data Expr

