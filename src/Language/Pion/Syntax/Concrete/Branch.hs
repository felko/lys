-- | AST of branches.
module Language.Pion.Syntax.Concrete.Branch
  ( Branch (..),
    Branches,
    Conjunction (..),
  )
where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan

data Branch c a = Branch
  { branchCase :: Located c,
    branchValue :: Located a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

type Branches c a = [Branch c a]

instance {-# OVERLAPPING #-} (Pretty c, Pretty a) => Pretty (Branches c a) where
  pretty items =
    prettyASTList
      [ prettyField (pretty branchCase) (pretty branchValue)
        | Branch {..} <- items
      ]

-- | A conjunction of items.
-- Used for conjunctive connectives, to support both
-- ordered and record types.
data Conjunction a
  = OrderedConjunction (Branches () a)
  | LabelledConjunction (Branches Label a)
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Conjunction a) where
  pretty = \case
    OrderedConjunction items ->
      prettyASTList [pretty branchValue | Branch {..} <- items]
    LabelledConjunction fields ->
      pretty fields
