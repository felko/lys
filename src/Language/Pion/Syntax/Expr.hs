-- | Syntax of expressions
module Language.Pion.Syntax.Expr (Expr (..)) where

import Language.Pion.Name (Name)
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Literal (Literal)

data Expr
  = App (Located Expr) (Located Expr)
  | Var Name
  | Abs (Located Name) (Located Expr)
  | Lit Literal
  deriving (Eq, Ord, Show)
