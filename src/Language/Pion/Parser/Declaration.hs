-- | Parser for type, process, and function declarations.
module Language.Pion.Parser.Declaration
  ( typeDeclaration,
    processDeclaration,
    functionDeclaration,
  )
where

import qualified Language.Pion.Lexer.Token as Token
import Language.Pion.Parser.Expression (expression)
import Language.Pion.Parser.Monad
import Language.Pion.Parser.Process (process)
import Language.Pion.Parser.Type (type')
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Declaration

typeDeclaration :: Parser (Located TypeDeclaration)
typeDeclaration = undefined

processDeclaration :: Parser (Located ProcessDeclaration)
processDeclaration = undefined

functionDeclaration :: Parser (Located FunctionDeclaration)
functionDeclaration = undefined
