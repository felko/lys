-- | Syntax of patterns
module Language.Pion.Syntax.Pattern (Pattern (..)) where

import Language.Pion.Name

data Pattern
  = Select Name Label
  | Extract Name
  | Unwrap Name
  | Copy
  | Unit
  deriving (Eq, Show)
