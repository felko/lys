{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Program where

import Language.Lys.Parser.AST hiding (moduleName)
import Language.Lys.Parser.Declaration
import Language.Lys.Parser.Process
import Language.Lys.Parser.Type
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative
import Control.Monad

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

program :: Parser Program
program = do
    keyword "module"
    mod <- lexeme moduleName
    exps <- parens (commaSep export)
    space
    imps <- many import'
    decls <- many declaration
    eof
    pure (Program mod exps imps decls)

export, moduleE, typeE, processE :: Parser Export
export = moduleE <|> typeE <|> processE <?> "export"

moduleE = keyword "module" >> ModuleE <$> moduleName
typeE = TypeE <$> upperIdentifier
processE = ProcessE <$> identifier

import' :: Parser Import
import' = keyword "import" >> ImportI <$> moduleName

moduleName :: Parser ModuleName
moduleName = lexeme $ upperIdentifier `Mega.sepBy1` char '.' 

