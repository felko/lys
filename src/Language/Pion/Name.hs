-- | Names
module Language.Pion.Name
  ( Name (..),
    Identifier (..),
    Label (..),
  )
where

import Language.Pion.Pretty

newtype Name = Name {unName :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)

newtype Label = Label {unLabel :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)
