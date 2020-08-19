-- | Syntax of top level declarations.
module Language.Pion.Syntax.Concrete.Declaration
  ( Declaration (..),
    TypeDeclaration (..),
    ProcessDeclaration (..),
    FunctionDeclaration (..),
  )
where

import Language.Pion.Name
import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Concrete.Expression (Expression)
import Language.Pion.Syntax.Concrete.Process (Process)
import Language.Pion.Syntax.Concrete.Type

-- | AST of declarations of any kind.
data Declaration
  = TypeDecl TypeDeclaration
  | ProcDecl ProcessDeclaration
  | FuncDecl FunctionDeclaration
  deriving (Eq, Show)

instance Pretty Declaration where
  pretty = \case
    TypeDecl typeDecl -> pretty typeDecl
    ProcDecl procDecl -> pretty procDecl
    FuncDecl funcDecl -> pretty funcDecl

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
  deriving (Eq, Show)

instance Pretty FunctionDeclaration where
  pretty FunctionDeclaration {..} =
    prettyASTNode
      "FunctionDeclaration"
      [ ("name", pretty funcDeclName),
        ("type", pretty funcDeclType),
        ("body", pretty funcDeclBody)
      ]
