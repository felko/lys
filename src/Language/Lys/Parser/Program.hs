{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Rule.Program
    ( program
    ) where

import Language.Lys.Parser.Rule.Expr
import Language.Lys.Parser.Rule.ChanExpr
import Language.Lys.Parser.Rule.Process
import Language.Lys.Parser.Tree.Program
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Type

import Text.Megaparsec as Mega

program :: LysParser Program
program =  space >> Program
       <$> (keyword "module" *> moduleDecl)
       <*> optional exportsDecl
       <*> many importDecl
       <*> (concat <$> many (nonIndented decls))

moduleDecl :: LysParser Module'
moduleDecl = lexeme $ do
    ns <- upperIdentifier `sepBy1` operator "."
    return $ Module' (init ns) (last ns)

exportsDecl :: LysParser [Export]
exportsDecl = lexeme $ parens (commaSep (space *> lexeme export))

export :: LysParser Export
export =  typeExport
      <|> processExport
      <?> "export"

typeExport, processExport :: LysParser Export
typeExport = do
    n <- upperIdentifier
    ms <- optional $ parens $ commaSep memberExport
    return $ TypeExport n ms

processExport = ProcessExport <$> identifier

memberExport :: LysParser MemberExport
memberExport =  conExport
            <|> fieldExport
            <?> "member export"

conExport, fieldExport :: LysParser MemberExport
conExport = ConExport <$> upperIdentifier
fieldExport = FieldExport <$> identifier

importDecl :: LysParser Import
importDecl = nonIndented $ do
    keyword "import"
    m <- lexeme moduleDecl
    exs <- option [] exportsDecl
    alias <- optional $ keyword "as" *> lexeme upperIdentifier
    return $ Import m exs alias

decls :: LysParser [Decl]
decls = typeDecl
    <|> try processDecls
    <|> processDef
    <?> "declaration"

typeDecl, processDecls, processDef :: LysParser [Decl]
typeDecl = return <$> do
    keyword "type"
    n <- lexeme upperIdentifier
    operator "="
    pos <- indentLevel
    indentGuard GT pos <|> indentGuard EQ pos
    c <- chanExpr
    return $ TypeDecl n c

processDecls = do
    ns <- commaSep1 (try identifier)
    operator ":"
    pos <- indentLevel
    indentGuard GT pos <|> indentGuard EQ pos
    c <- chanExpr
    return $ map (flip ProcessDecl c) ns

processDef = return <$> do
    c <- optional $ notAttr <* operator "."
    n <- identifier
    cs <- many (lexeme notAttr)
    let cs' = case c of
                  Just c' -> c':cs
                  Nothing -> cs
    pos <- indentLevel
    operator "="
    indentGuard GT pos <|> indentGuard EQ pos
    p <- process
    return $ ProcessDef n cs' p