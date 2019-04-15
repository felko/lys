{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Process where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Name
import Language.Lys.Parser.Type
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative
import Control.Monad

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

import Debug.Trace

process, parFactor :: Parser Process
process =  try (foldr1 ParP <$> parSep2 parFactor)
       <|> contractP
       <|> parFactor
       <?> "process"

parFactor =  braces process
         <|> nilP
         <|> newP
         <|> matchP
         <|> try callP
         <|> try inputP
         <|> try outputP
         <|> try injectP
         <|> try replicateP
         <?> "process"

nilP, newP, inputP, outputP, replicateP, matchP, injectP, sourceP, contractP, callP :: Parser Process

nilP = NilP <$ keyword "end"

newP = do
    keyword "new"
    x <- identifier
    mt <- optional (symbol ":" *> type')
    p <- braces process
    pure (NewP x mt p)

inputP = do
    x <- name
    symbol "?"
    y <- parens identifier
    symbol ","
    p <- parFactor
    pure (InputP x y p)

outputP = do
    x <- name
    symbol "!"
    y <- parens name
    symbol ","
    p <- parFactor
    pure (OutputP x y p)

replicateP = do
    keyword "repeat"
    x <- name
    symbol "?"
    y <- parens identifier
    symbol ","
    p <- parFactor
    pure (ReplicateP x y p)

injectP = do
    x <- name
    symbol "."
    l <- identifier
    symbol "!"
    y <- parens name <|> pure (LitN OneL)
    p <- lexeme $ (symbol "," *> parFactor) <|> pure NilP
    pure (InjectP x l y p)

matchP = do
    keyword "match"
    x <- name
    bs <- braces (parSep branch)
    pure (MatchP x bs)

sourceP = do
    keyword "repeat"
    x <- identifier
    symbol "!"
    y <- parens identifier
    symbol ","
    p <- parFactor
    pure (SourceP x y p)

contractP = do
    keyword "contract"
    x <- identifier
    y <- parens identifier
    p <- braces process
    symbol "|"
    q <- process
    pure (ContractP x y p q)

callP = CallP <$> identifier <*> parens (commaSep name)

pattern, varPat, wildcardPat :: Parser Pattern
pattern = varPat <|> wildcardPat <?> "pattern"

varPat = VarPat <$> identifier
wildcardPat = WildcardPat <$ symbol "_"

branch :: Parser (Branch Process)
branch = do
    l <- identifier
    y <- parens pattern <|> pure WildcardPat
    symbol ":"
    p <- parFactor
    pure (Branch l y p)
