{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Syntax of processes
module Language.Pion.Syntax.Process where

import Data.Some
import Language.Pion.Name
import Language.Pion.Syntax.Expr
import Language.Pion.Syntax.Pattern
import qualified Language.Pion.Syntax.Process.Flow as Flow

data PortMap = PortMap
  { portMapInputs :: HashMap Label Label,
    portMapOutputs :: HashMap Label Label
  }
  deriving (Eq, Show)

data Side = LeftSide | RightSide
  deriving (Eq, Ord, Show)

data Action (control :: Flow.Flow) where
  -- P; x ← e
  Post :: Name -> Expr -> Action 'Flow.Post
  -- p → x; P
  Pre :: Pattern -> Name -> Action 'Flow.Pre
  -- run p { x ← a, y → b }
  Run :: Ident -> PortMap -> Action 'Flow.Terminal
  -- join z ←/→ { x : P | y : Q }
  Join :: Name -> Side -> Action 'Flow.Branch
  -- match z ←/→ { x : P | y : Q }
  Match :: Name -> Side -> Action 'Flow.Branch

data Process = Process
  {procActions :: [Some Action]}
