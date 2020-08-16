-- | Syntax of expressions
module Language.Pion.Syntax.Expr (Expr (..)) where

import Language.Pion.Name (Name)
import Language.Pion.Pretty
import Language.Pion.SourceSpan (Located)
import Language.Pion.Syntax.Literal (Literal (..))

data Expr
  = App (Located Expr) (Located Expr)
  | Var Name
  | Abs (Located Name) (Located Expr)
  | Lit Literal
  deriving (Eq, Ord, Show)

instance Pretty Expr where
  pretty = \case
    App callee argument ->
      prettyASTNode
        "App"
        [ ("callee", pretty callee),
          ("argument", pretty argument)
        ]
    Var name ->
      prettyASTNode
        "Var"
        [ ("name", pretty name)
        ]
    Abs variable body ->
      prettyASTNode
        "Abs"
        [ ("variable", pretty variable),
          ("body", pretty body)
        ]
    Lit literal ->
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
