{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Rule.Expr
    ( expr
    , exprFactor, exprTerm, notAttr
    , varExpr, litExpr, attrExpr, callExpr, newExpr, conExpr, discardExpr
    ) where

import Language.Lys.Parser.Rule.Operator
import Language.Lys.Parser.Rule.Literal
import Language.Lys.Parser.Rule.ChanExpr
import Language.Lys.Parser.Tree.Expr
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Type
import Language.Lys.Annotation

import Data.Semigroup

import Control.Comonad.Cofree

import Control.Monad.Combinators.Expr

import Text.Megaparsec as Mega

import Lens.Micro.Platform

import Debug.Trace
import Language.Lys.Pretty hiding ((<>), parens)

-- | Operator table
operators :: [[Operator LysParser (Parsed Expr)]]
operators =
    [ [ prefix Not "not" ]
    , [ prefix Pos  "+",  prefix Neg  "-" ]
    , [ infixR Pow  "^" ]
    , [ infixL Mul  "*",  infixL Div  "/" ]
    , [ infixL Add  "+",  infixL Sub  "-" ]
    , [ infixN Eq  "==",  infixN Neq  "!="
      , infixN Ge  ">=",  infixN Gt   ">"
      , infixN Le  "<=",  infixN Lt   "<" ]
    , [ infixL And "and", infixL Or   "or" ]
    , [ infixN Bind ".=", infixN IAdd "+="
      , infixN ISub "-=", infixN IMul "*="
      , infixN IDiv "/=", infixN IPow "^=" ]
    ]

expr :: LysParser (Parsed Expr)
expr = makeExprParser (lexeme exprFactor) operators

exprFactor, exprTerm, notAttr :: LysParser (Parsed Expr)
exprFactor =  try callExpr
          <|> try conExpr
          <|> try newExpr
          <|> exprTerm
          <?> "expression"

exprTerm =  try attrExpr
        <|> notAttr
        <|> parens expr
        <?> "expression"

notAttr =  varExpr
       <|> litExpr
       <|> discardExpr
       <|> nullaryConExpr
       <?> "expression"

varExpr, litExpr, attrExpr, callExpr, newExpr, conExpr, discardExpr :: LysParser (Parsed Expr)
varExpr = label "varExpr" $ do
    (ident, s) <- spanned identifier
    return $ s :< VarExpr ident

litExpr = label "litExpr" $ do
    (lit, s) <- spanned literal
    return $ s :< LitExpr lit

attrExpr = label "attrExpr" $ do
    (ex, s) <- spanned notAttr
    pos <- indentLevel
    as <- some ( operator "."
              *> (indentGuard GT pos <|> indentGuard EQ pos)
              *> lexeme (spanned identifier) )
    return $ foldl (\ e' (a, e) -> (s <> e) :< AttrExpr e' a) ex as

callExpr = label "callExpr" $  try (indentBlock callBlock)

newExpr = label "newExpr" $ do
    (_, s) <- spanned $ keyword "new"
    pos <- indentLevel
    indentGuard GT pos <|> indentGuard EQ pos
    name <- identifier
    (bind, e) <- spanned . optional $ (operator ":" *> chanExpr)
    return $ (s <> e) :< NewExpr name bind

conExpr = label "conExpr" $ do
    (n, s) <- spanned upperIdentifier
    pos <- indentLevel
    indentGuard GT pos <|> indentGuard EQ pos
    (ps, e) <- spanned (commaSep1 exprTerm)
    return $ (s <> e) :< ConExpr n ps

discardExpr = label "discardExpr" $ do
    (_, s) <- spanned $ symbol "_"
    return $ s :< DiscardExpr

nullaryConExpr :: LysParser (Parsed Expr)
nullaryConExpr = label "nullaryConExpr" $ do
    (n, s) <- spanned upperIdentifier
    return $ s :< ConExpr n []

callBlock :: LysParser (IndentOpt LysParser (Parsed Expr) (Parsed Expr))
callBlock = do
    f <- lexeme exprTerm
    traceShow (prettyShow f) $ return () 
    pos <- indentLevel
    return $ IndentSome
        Nothing
        (\ as -> return $ (f ^. ann <> last as ^. ann) :< CallExpr f as)
        (lexeme exprTerm)
