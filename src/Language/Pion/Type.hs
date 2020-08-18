-- | Datatypes for representing logical symbols.
module Language.Pion.Type
  ( -- * Connectives
    ConnectiveType (..),
    connectiveTypeSymbol,

    -- * Modalities
    ModalityType (..),
    modalityTypeSymbol,

    -- * Units
    unitTypeSymbol,
  )
where

-- | Type of n-ary logical connectives.
data ConnectiveType
  = Tensor
  | Par
  | Plus
  | With
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The symbol associated with a logical connective.
connectiveTypeSymbol :: ConnectiveType -> Text
connectiveTypeSymbol = \case
  Tensor -> "⨂"
  Par -> "⅋"
  Plus -> "⨁"
  With -> "&"

-- | Type of unary logical connectives (exponentials).
data ModalityType
  = WhyNot
  | OfCourse
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The symbol associated with modalities.
modalityTypeSymbol :: ModalityType -> Text
modalityTypeSymbol = \case
  WhyNot -> "?"
  OfCourse -> "!"

-- | The symbol associated with a logical unit.
unitTypeSymbol :: ConnectiveType -> Text
unitTypeSymbol = \case
  Tensor -> "end"
  Par -> "⊥"
  Plus -> "void"
  With -> "⊤"
