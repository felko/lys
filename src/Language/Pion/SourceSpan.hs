{-# LANGUAGE TemplateHaskell #-}

-- | Intervals of source positions
module Language.Pion.SourceSpan
  ( -- * Source interval
    SourceSpan (..),
    Located (..),
    unknownLocation,
  )
where

import Data.Functor.Classes
import GHC.Generics
import Generic.Data (Generically1 (..))
import Language.Pion.Orphans ()
import Prettyprinter
import qualified Text.Megaparsec.Pos as Mega

-- | A position interval in a source file.
data SourceSpan = SourceSpan
  { spanStart :: !Mega.SourcePos,
    spanEnd :: !Mega.SourcePos
  }
  deriving (Eq, Ord, Show)
  deriving (Generic, Hashable)

instance Semigroup SourceSpan where
  SourceSpan start end <> SourceSpan start' end' =
    SourceSpan (min start start') (max end end')

-- | An item together with optional source location information
data Located a = Located
  { locNode :: a,
    locSpan :: !(Option SourceSpan)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving (Generic, Generic1, Hashable)
  deriving (Show1) via (Generically1 Located)

unknownLocation :: a -> Located a
unknownLocation node = Located node mempty

instance Eq1 Located where
  liftEq eq (Located node span) (Located node' span') =
    (node `eq` node') && (span == span')

instance Ord1 Located where
  liftCompare cmp (Located node span) (Located node' span') =
    (node `cmp` node') <> (span `compare` span')

instance Applicative Located where
  pure x = Located x mempty
  Located f fSpan <*> Located x xSpan = Located (f x) (fSpan <> xSpan)

instance Pretty a => Pretty (Located a) where
  pretty = pretty . locNode
