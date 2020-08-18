-- | Syntax of patterns
module Language.Pion.Syntax.Pattern (Pattern (..)) where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Branch

data Pattern
  = -- | Identity
    Variable Name
  | -- | Elimination of "tensor"
    Split (Conjunction Pattern)
  | -- | Elimination of "with"
    Select (Located Label) (Located Pattern)
  | -- | Dereliction of "of course"
    Extract (Located Pattern)
  | -- | Dereliction of "why not"
    Unwrap (Located Pattern)
  | -- | Contraction or weakening of "of course"
    Copy [Located Pattern]
  deriving (Eq, Ord, Show)

instance Pretty Pattern where
  pretty = \case
    Variable name -> prettyLabelled "Variable" $ pretty name
    Split elements ->
      prettyLabelled "Split" $ pretty elements
    Select label pattern' ->
      prettyASTNode
        "Select"
        [ ("label", pretty label),
          ("pattern", pretty pattern')
        ]
    Extract pattern' ->
      prettyLabelled "Extract" $ pretty pattern'
    Unwrap pattern' ->
      prettyLabelled "Unwrap" $ pretty pattern'
    Copy copies ->
      prettyLabelled "Copy" $ prettyASTList (pretty <$> copies)
