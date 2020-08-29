-- | Common syntactic representations between multiple passes.
module Language.Pion.Syntax.Common
  ( Side (..),
    PortMap,
    Branch (..),
    Branches (..),
    branchesToHashMap,
    Conjunction (..),
    conjunctionToBranches,
    conjunctionToHashMap,
    Literal (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan

data Side = LeftSide | RightSide
  deriving (Eq, Ord, Show)

type PortMap = [Located (Name, Side, Name)]

instance {-# OVERLAPPING #-} Pretty PortMap where
  pretty =
    let prettyPort Located {locNode = (p, s, q)} =
          case s of
            LeftSide -> pretty p <+> "←" <+> pretty q
            RightSide -> pretty p <+> "→" <+> pretty q
     in prettyLabelled "PortMap"
          . prettySyntaxList
          . fmap prettyPort

data Branch c a = Branch
  { branchCase :: Located c,
    branchValue :: Located a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance Bifunctor Branch where
  bimap f g (Branch c v) = Branch (f <$> c) (g <$> v)

instance Bifoldable Branch where
  bifoldMap f g (Branch c v) = f (locNode c) <> g (locNode v)

instance Bitraversable Branch where
  bitraverse f g (Branch c v) = Branch <$> traverse f c <*> traverse g v

newtype Branches c a = Branches
  {getBranches :: [Branch c a]}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

branchesToHashMap ::
  (Eq c, Hashable c) =>
  Branches c a ->
  HashMap.HashMap (Located c) (Located a)
branchesToHashMap (Branches branches) =
  HashMap.fromList
    [ (branchCase, branchValue)
      | Branch {..} <- branches
    ]

instance Bifunctor Branches where
  bimap f g (Branches items) = Branches (bimap f g <$> items)

instance Bifoldable Branches where
  bifoldMap f g (Branches items) = foldMap (bifoldMap f g) items

instance Bitraversable Branches where
  bitraverse f g (Branches items) = Branches <$> traverse (bitraverse f g) items

instance (Pretty c, Pretty a) => Pretty (Branches c a) where
  pretty (Branches items) =
    prettySyntaxList
      [ prettySyntaxField (pretty branchCase) (pretty branchValue)
        | Branch {..} <- items
      ]

-- | A conjunction of items.
-- Used for conjunctive connectives, to support both
-- ordered and record types.
data Conjunction a
  = OrderedConjunction (Branches () a)
  | LabelledConjunction (Branches Name a)
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

conjunctionToBranches :: Conjunction a -> Branches Name a
conjunctionToBranches (OrderedConjunction items) =
  Branches $
    zipWith
      Branch
      [ unknownLocation (Name (show i))
        | i <- [1 :: Int ..]
      ]
      (branchValue <$> getBranches items)
conjunctionToBranches (LabelledConjunction branches) = branches

conjunctionToHashMap :: Conjunction a -> HashMap.HashMap (Located Name) (Located a)
conjunctionToHashMap = branchesToHashMap . conjunctionToBranches

instance Pretty a => Pretty (Conjunction a) where
  pretty = \case
    OrderedConjunction items ->
      prettySyntaxList [pretty branchValue | Branch {..} <- getBranches items]
    LabelledConjunction fields ->
      pretty fields

data Literal
  = IntL Integer
  | FloatL Double
  | CharL Char
  | StringL Text
  deriving (Eq, Ord, Show)

instance Pretty Literal where
  pretty = \case
    IntL n -> pretty n
    FloatL x -> pretty x
    CharL c -> prettyCharLiteral c
    StringL s -> prettyStringLiteral s
