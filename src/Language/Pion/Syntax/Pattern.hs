-- | Syntax of patterns
module Language.Pion.Syntax.Pattern (Pattern (..)) where

import Language.Pion.Name
import Language.Pion.SourceSpan

data Pattern
  = Select (Located Name) (Located Label)
  | Extract (Located Name)
  | Unwrap (Located Name)
  | Copy
  | Unit
  deriving (Eq, Ord, Show)
