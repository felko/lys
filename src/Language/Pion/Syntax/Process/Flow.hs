-- | Control flow in processes
module Language.Pion.Syntax.Process.Flow (Flow (..)) where

data Flow
  = Pre
  | Post
  | Branch
  | Terminal
  deriving (Eq, Show)
