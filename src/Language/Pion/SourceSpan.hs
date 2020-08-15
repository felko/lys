-- | Intervals of source positions
module Language.Pion.SourceSpan
  ( -- * Source interval
    SourceSpan (..),
    Located (..),
  )
where

import qualified Text.Megaparsec.Pos as Mega

-- | A position interval in a source file.
data SourceSpan = SourceSpan
  { spanStart :: !Mega.SourcePos,
    spanStop :: !Mega.SourcePos
  }
  deriving (Eq, Ord, Show)

instance Semigroup SourceSpan where
  SourceSpan start stop <> SourceSpan start' stop' =
    SourceSpan (min start start') (max stop stop')

-- | An item together with optional source location information
data Located a = Located
  { locNode :: a,
    locSpan :: !(Option SourceSpan)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Located where
  pure x = Located x mempty
  Located f fSpan <*> Located x xSpan = Located (f x) (fSpan <> xSpan)
