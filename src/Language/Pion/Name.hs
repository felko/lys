-- | Names
module Language.Pion.Name
  ( Name (..),
    Ident (..),
    Label (..),
  )
where

newtype Name = Name {getName :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable)

newtype Ident = Ident {getIdent :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable)

newtype Label = Label {getLabel :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Hashable)
