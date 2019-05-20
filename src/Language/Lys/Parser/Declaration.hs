{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Declaration where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Process
import Language.Lys.Parser.Type
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative
import Control.Monad

import Control.Lens hiding (Context)

import qualified Data.Set as Set

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

declaration, typeD, processD :: Parser Declaration
declaration = typeD <|> processD <?> "declaration"

typeD = do
    keyword "type"
    n <- upperIdentifier
    ts <- angles (commaSep typeParam) <|> pure []
    symbol "="
    t <- withParserState (rigidTypeVars .~ Set.fromList (map (view tpName) ts)) type'
    pure (TypeD n ts t)

processD = do
    keyword "process"
    n <- identifier
    ts <- angles (commaSep typeParam) <|> pure []
    (ns, p) <- withParserState (rigidTypeVars .~ Set.fromList (map (view tpName) ts)) do
        ns <- parens (commaSep nameParam)
        p <- braces process
        pure (ns, p)
    pure (ProcessD n ts ns p)

typeParam, paramTP, constrTP :: Parser TypeParam
typeParam = try constrTP <|> paramTP <?> "type parameter"

paramTP = ParamTP <$> upperIdentifier

constrTP = do
    n <- identifier
    symbol ":"
    c <- constraint
    pure (ConstrTP n c)

nameParam, inferredNP, annotatedNP :: Parser NameParam
nameParam = try annotatedNP <|> inferredNP <?> "parameter"

inferredNP = InferredNP <$> identifier

annotatedNP = do
    n <- identifier
    symbol ":"
    t <- type'
    pure (AnnotatedNP n t)

constraint :: Parser Constraint
constraint = pure NoneC
