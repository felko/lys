-- | Syntax of literals
module Language.Pion.Syntax.Concrete.Literal (Literal (..)) where

import Language.Pion.Pretty

data Literal
  = IntegerLiteral Integer
  | FloatLiteral Double
  | CharLiteral Char
  | StringLiteral Text
  deriving (Eq, Ord, Show)

instance Pretty Literal where
  pretty = \case
    IntegerLiteral n -> pretty n
    FloatLiteral x -> pretty x
    CharLiteral c -> prettyCharLiteral c
    StringLiteral s -> prettyStringLiteral s
