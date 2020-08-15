-- | Control flow in processes
module Language.Pion.Syntax.Process.Flow (Flow (..)) where

data Flow
  = Pre
  | Post
  | Terminal
  deriving (Eq, Show)
