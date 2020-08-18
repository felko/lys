-- | Parser for type, process, and function declarations.
module Language.Pion.Parser.Declaration
  ( declaration,
    typeDeclaration,
    processDeclaration,
    functionDeclaration,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Parser.Expression (expression)
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Process (process)
import Language.Pion.Parser.Type (sequent, type')
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Declaration

declaration :: Parser (Located Declaration)
declaration =
  locatedDecl TypeDecl typeDeclaration
    <|> locatedDecl ProcDecl processDeclaration
    <|> locatedDecl FuncDecl functionDeclaration
  where
    locatedDecl constr declParser = fmap constr <$> declParser

typeDeclaration :: Parser (Located TypeDeclaration)
typeDeclaration = located do
  keyword Token.Type
  typeDeclName <- located identifier
  punctuation Token.Equal
  typeDeclType <- type'
  pure TypeDeclaration {..}

processDeclaration :: Parser (Located ProcessDeclaration)
processDeclaration = located do
  keyword Token.Proc
  procDeclName <- located identifier
  punctuation Token.DoubleColon
  procDeclType <- sequent
  procDeclBody <- between Token.Brace process
  pure ProcessDeclaration {..}

functionDeclaration :: Parser (Located FunctionDeclaration)
functionDeclaration = located do
  keyword Token.Func
  funcDeclName <- located identifier
  punctuation Token.Colon
  funcDeclType <- type'
  funcDeclBody <- between Token.Brace expression
  pure FunctionDeclaration {..}
