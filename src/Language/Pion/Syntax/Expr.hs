-- | Syntax of expressions

module Language.Pion.Syntax.Expr (Expr(..)) where

import Language.Pion.Name

data Expr
  = App Expr Expr
    -- ^ f g
  | Var Name
    -- ^ x
  | Lam Name Expr
    -- λ x ⊸ e
