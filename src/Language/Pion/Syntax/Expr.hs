-- | Syntax of expressions
module Language.Pion.Syntax.Expr (Expr (..)) where

import Language.Pion.Name (Name)
import Language.Pion.SourceSpan (Located)

data Expr
  = App (Located Expr) (Located Expr)
  | Var Name
  | Abs (Located Name) (Located Expr)
  deriving (Eq, Ord, Show)
