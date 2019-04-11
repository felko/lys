{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Types where

import qualified Data.Map as Map

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
    | PrimT PrimType
    deriving (Eq, Ord, Show)

data PrimType = IntT
    deriving (Eq, Ord, Show)
