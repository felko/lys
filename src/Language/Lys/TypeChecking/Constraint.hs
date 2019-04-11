module Language.Lys.TypeChecker.Constraint where

data Constraint
    = TypeC TypeConstraint
    | EnvC EnvConstraint
    | 

data 

data TypeConstraint
    = EqTC Type Type
    | OutputTC Type Type Type
    | InputTC Type Type Type
    
