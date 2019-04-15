{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Types where

import qualified Data.Map as Map

import Control.Lens

-- ≡≅⊗⊕⊥⊤⊢⊣⋁⋀∝∈∉⎜⊸⅋&ΔΓν

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
    = IntL Integer
    | OneL
    deriving (Eq, Ord, Show)

data Branch a = Branch
    { _branchLabel   :: String
    , _branchPatern  :: Pattern
    , _branchProcess :: a }
    deriving (Eq, Show)
makeLenses ''Branch

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
