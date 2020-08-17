-- | AST of expressions.
module Language.Pion.Syntax.Expression (Expression (..)) where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Branch
import Language.Pion.Syntax.Literal (Literal (..))
import Language.Pion.Syntax.Pattern (Pattern)

data Expression
  = -- | Identity
    Variable Name
  | -- | Introduction of "lollipop"
    Abstraction (Located Name) (Located Expression)
  | -- | Elimination of "lollipop"
    Application (Located Expression) (Located Expression)
  | -- | A literal value
    Literal Literal
  | -- | Introduction of "tensor"
    Tuple (Conjunction Expression)
  | -- | Introduction of "with"
    Alternative (Conjunction Expression)
  | -- | Elimination of "plus"
    Match (Located Expression) (Branches Pattern Expression)
  | -- | Let-binding
    Let (Branches Pattern Expression) (Located Expression)
  deriving (Eq, Ord, Show)

instance Pretty Expression where
  pretty = \case
    Application callee argument ->
      prettyASTNode
        "Application"
        [ ("callee", pretty callee),
          ("argument", pretty argument)
        ]
    Variable name ->
      prettyASTNode
        "Variable"
        [ ("name", pretty name)
        ]
    Abstraction variable body ->
      prettyASTNode
        "Abstraction"
        [ ("variable", pretty variable),
          ("body", pretty body)
        ]
    Literal literal ->
      let literalType = case literal of
            IntegerLiteral {} -> "integer"
            FloatLiteral {} -> "float"
            CharLiteral {} -> "character"
            StringLiteral {} -> "string"
       in prettyASTNode
            "Literal"
            [ ("type", literalType),
              ("value", pretty literal)
            ]
    Tuple fields ->
      prettyLabelled "Tuple" (pretty fields)
    Alternative cases ->
      prettyLabelled "Alternative" (pretty cases)
    Match value cases ->
      prettyASTNode
        "Match"
        [ ("matched", pretty value),
          ("cases", pretty cases)
        ]
    Let bindings body ->
      prettyASTNode
        "Let"
        [ ("bindings", pretty bindings),
          ("body", pretty body)
        ]
