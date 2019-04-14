{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Types where

import qualified Data.Map as Map

import Data.Deriving

-- ≡≅⊗⊕⊥⊤⊢⊣⋁⋀∝∈∉⎜⊸⅋&ΔΓν

-- | The core representation of processes
data Process
    = NilP
    | ParP Process Process
    | NewP String (Maybe Type) Process
    | OutputP Name Name Process
    | InputP Name String Process
    | ReplicateP Name String Process
    | InjectP Name String Name Process
    | CaseP Name [Branch]
    | CallP String [Name]
    | SourceP String String Process
    | ContractP String String Process Process
    deriving (Eq, Show)

data Branch = Branch String Pattern Process
    deriving (Eq, Show)

data Pattern
    = VarPat String
    -- | LitPat Literal
    | WildcardPat
    deriving (Eq, Ord, Show)

data Name
    = VarN String
    | LitN Literal
    deriving (Eq, Ord, Show)

data Literal
    = IntL Int
    | OneL
    deriving (Eq, Ord, Show)

type Fields = Map.Map String Type

data Type
    = TopT | BottomT
    | OneT | ZeroT
    | OfCourseT Type
    | WhyNotT Type
    | TensorT Type Type
    | ParT Type Type
    | PlusT Fields
    | WithT Fields
    | DualT Type
    | VarT String
    | IdentT String
    | AppT Type [Type]
    | PrimT PrimType
    deriving (Eq, Ord, Show)

data PrimType = IntT
    deriving (Eq, Ord, Show)
