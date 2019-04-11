{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Types where

import Data.Deriving

-- ≡≅⊗⊕⊥⊤⊢⊣⋁⋀∝∈∉⎜⊸⅋&ΔΓν

-- | The core representation of processes
data Process
    = NilP
    | ParP Process Process
    | NewP String Type Process
    | OutputP Name Name Process
    | InputP Name String Process
    | ReplicateP Name String Process
    | InjectP Name String Process
    | CaseP Name [(String,Process)]
    | VarP String
    | AppP String Name
    | SourceP String String Process
    | ContractP String String Process Process
    deriving (Eq, Show)

data Name
    = VarN String
    | LitN Literal
    deriving (Eq, Show)

data Literal
    = IntL Int
    deriving (Eq, Show)

-- type Fields = [(String, Type)]

data Type
    = TopT | BottomT
    | OneT | ZeroT
    | OfCourseT Type
    | WhyNotT Type
    | TensorT Type Type
    | ParT Type Type
    | PlusT Type Type
    | WithT Type Type
    | DualT Type
    | VarT String
    | PrimT PrimType
    deriving (Eq, Ord, Show)

data PrimType = IntT
    deriving (Eq, Ord, Show)
