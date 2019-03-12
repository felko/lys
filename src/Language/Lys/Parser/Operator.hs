{-# LANGUAGE
    OverloadedStrings
  #-}

module Language.Lys.Parser.Rule.Operator
    ( infixL, infixR, infixN
    , prefix, postfix
    ) where

import Language.Lys.Parser.Tree.Expr
import Language.Lys.Parser.Tree.Operator
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Type
import Language.Lys.Annotation

import Data.Semigroup
import Control.Applicative
import Control.Comonad.Cofree

import Control.Monad.Combinators.Expr

import qualified Text.Megaparsec as Mega

import qualified Data.Text as Text

import Lens.Micro.Platform

infixL, infixR, infixN :: BinOp -> Text.Text -> Operator LysParser (Parsed Expr)
infixL op sym = InfixL (parseBinOp op sym)
infixR op sym = InfixR (parseBinOp op sym)
infixN op sym = InfixN (parseBinOp op sym)

parseBinOp :: BinOp -> Text.Text -> LysParser (Parsed Expr -> Parsed Expr -> Parsed Expr)
parseBinOp op sym = do
    pos <- indentLevel
    operator sym
    indentGuard GT pos <|> indentGuard EQ pos
    return $ \ lhs rhs ->
        (lhs ^. ann <> rhs ^. ann) :< BinOpExpr op lhs rhs

prefix, postfix :: UnOp -> Text.Text -> Operator LysParser (Parsed Expr)
prefix op sym = Prefix $ do
    pos <- indentLevel
    (_, s) <- spanned (operator sym)
    indentGuard GT pos <|> indentGuard EQ pos
    return $ \ val -> (s <> val ^. ann) :< UnOpExpr op val
postfix op sym = Postfix $ do
    pos <- indentLevel
    (_, e) <- spanned (operator sym)
    indentGuard GT pos <|> indentGuard EQ pos
    return $ \ val -> (val ^. ann <> e) :< UnOpExpr op val