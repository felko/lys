-- | Datatypes for representing logical symbols.
module Language.Pion.Type
  ( -- * Connectives
    ConnectiveType (..),
    connectiveTypeSymbol,

    -- * Modalities
    ModalityType (..),
    modalityTypeSymbol,

    -- * Units
    UnitType (..),
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

-- | Type of logical units.
data UnitType
  = Bottom
  | Top
  | Void
  | End
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The symbol associated with a logical unit.
unitTypeSymbol :: UnitType -> Text
unitTypeSymbol = \case
  Bottom -> "⊥"
  Top -> "⊤"
  Void -> "void"
  End -> "end"
