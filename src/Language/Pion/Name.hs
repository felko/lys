-- | Names
module Language.Pion.Name
  ( Name (..),
    Ident (..),
    Label (..),
  )
where

import Language.Pion.Pretty

newtype Name = Name {getName :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)

newtype Ident = Ident {getIdent :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)

newtype Label = Label {getLabel :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable, Pretty)
