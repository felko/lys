-- | Syntax of types.
module Language.Pion.Syntax.Concrete.Type
  ( Type(..),
    Context,
    Sequent(..),
  )
where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Concrete.Branch
import Language.Pion.Type

-- | AST of types.
data Type
  = Composite ConnectiveType (Conjunction Type)
  | Unit ConnectiveType
  | Modality ModalityType (Located Type)
  | Variable Identifier
  deriving (Eq, Show)

instance Pretty Type where
  pretty = \case
    Composite connective types ->
      prettyASTNode
        "Composite"
        [ ("connective", pretty (show @Text connective)),
          ("types", pretty types)
        ]
    Unit connective ->
      prettyLabelled "Unit" $ pretty (show @Text connective)
    Modality modality type' ->
      prettyASTNode
        "Modality"
        [ ("modality", pretty (show @Text modality)),
          ("type", pretty type')
        ]
    Variable identifier ->
      prettyLabelled "Variable" $ pretty identifier

-- | A typing context.
type Context = Branches Name Type

-- | Process types.
data Sequent = Sequent
  { sequentAntecedents :: Context,
    sequentSuccedents :: Context
  }
  deriving (Eq, Show)

instance Pretty Sequent where
  pretty Sequent {..} =
    prettyASTNode
      "Sequent"
      [ ("antecedents", pretty sequentAntecedents),
        ("succedents", pretty sequentSuccedents)
      ]
