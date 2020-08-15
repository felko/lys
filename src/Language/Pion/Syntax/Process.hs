{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Syntax of processes
module Language.Pion.Syntax.Process where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some
import Language.Pion.Name (Ident, Label, Name)
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Expr
import Language.Pion.Syntax.Pattern
import qualified Language.Pion.Syntax.Process.Flow as Flow

data Side = LeftSide | RightSide
  deriving (Eq, Ord, Show)

type PortMap = [Located (Label, Label, Side)]

data Branch = Branch
  { branchLabel :: Label,
    branchBody :: Located Process
  }
  deriving (Eq, Ord, Show)

type Branches = [Located Branch]

data Action (control :: Flow.Flow) where
  -- P; x ← e
  Intro :: Located Name -> Located Expr -> Action 'Flow.Post
  -- p → x; P
  Pre :: Located Pattern -> Located Name -> Action 'Flow.Pre
  -- run p { x ← a, y → b }
  Run :: Located Ident -> Located PortMap -> Action 'Flow.Terminal
  -- join z ←/→ { x : P | y : Q }
  Join :: Located Name -> Side -> Located Branches -> Action 'Flow.Terminal
  -- match z ←/→ { x : P | y : Q }
  Match :: Located Name -> Side -> Located Branches -> Action 'Flow.Terminal

deriving instance Eq (Action f)

deriving instance Ord (Action f)

deriving instance Show (Action f)

data Process = Process
  {procActions :: [Some Action]}

deriving instance Eq Process

deriving instance Ord Process

deriving instance Show Process

deriveGEq ''Action
deriveGCompare ''Action
deriveGShow ''Action
