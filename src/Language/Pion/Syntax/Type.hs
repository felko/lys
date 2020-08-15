-- | Syntax of types
module Language.Pion.Syntax.Type where

import Data.Vector (Vector)
import Language.Pion.Name

data CompositeConnective
  = Tensor
  | Par
  | Plus
  | With
  deriving (Eq, Show)

data ExponentialConnective = OfCourse | WhyNot
  deriving (Eq, Show)

data Components
  = Unit
  | Ordered (Vector Type)
  | Row (HashMap Label Type)
  deriving (Eq, Show)

data Type
  = Composite CompositeConnective Components
  | Modality ExponentialConnective Type
  deriving (Eq, Show)
