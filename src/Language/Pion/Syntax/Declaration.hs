-- | Syntax of top level declarations.
module Language.Pion.Syntax.Declaration
  ( TypeDeclaration (..),
    ProcessDeclaration (..),
    FunctionDeclaration (..),
  )
where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Expression (Expression)
import Language.Pion.Syntax.Process (Process)
import Language.Pion.Syntax.Type

-- |  AST of type declarations.
data TypeDeclaration = TypeDeclaration
  { typeDeclName :: Located Identifier,
    typeDeclType :: Located Type
  }
  deriving (Eq, Show)

instance Pretty TypeDeclaration where
  pretty TypeDeclaration {..} =
    prettyASTNode
      "TypeDeclaration"
      [ ("name", pretty typeDeclName),
        ("type", pretty typeDeclType)
      ]

-- | AST of process declarations.
data ProcessDeclaration = ProcessDeclaration
  { procDeclName :: Located Identifier,
    procDeclType :: Located Sequent,
    procDeclBody :: Located Process
  }
  deriving (Eq, Show)

instance Pretty ProcessDeclaration where
  pretty ProcessDeclaration {..} =
    prettyASTNode
      "ProcessDeclaration"
      [ ("name", pretty procDeclName),
        ("type", pretty procDeclType),
        ("body", pretty procDeclBody)
      ]

-- | AST of function declarations.
data FunctionDeclaration = FunctionDeclaration
  { funcDeclName :: Located Identifier,
    funcDeclType :: Located Type,
    funcDeclBody :: Located Expression
  }

instance Pretty FunctionDeclaration where
  pretty FunctionDeclaration {..} =
    prettyASTNode
      "FunctionDeclaration"
      [ ("name", pretty funcDeclName),
        ("type", pretty funcDeclType),
        ("body", pretty funcDeclBody)
      ]
