-- | Syntax of literals
module Language.Pion.Syntax.Literal (Literal (..)) where

data Literal
  = IntegerLiteral Integer
  | FloatLiteral Double
  | CharLiteral Char
  | StringLiteral Text
  deriving (Eq, Ord, Show)
