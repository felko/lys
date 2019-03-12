{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Rule.Process
    ( strategy
    , process
    , parProc, conProc, matchProc, ifProc, whileProc, forProc, exprProc, doneProc
    ) where

import Language.Lys.Parser.Rule.Expr     (expr)
import Language.Lys.Parser.Rule.ChanExpr (chanExpr)
import Language.Lys.Parser.Tree.Expr
import Language.Lys.Parser.Tree.Process
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Type
import Language.Lys.Annotation

import Control.Comonad.Cofree

import Data.Semigroup
import Data.List (foldl')

import Lens.Micro.Platform

import Control.Monad.Combinators

import qualified Text.Megaparsec as Mega

strategy :: LysParser Strategy
strategy =  (Sequential <$ symbol ",")
        <|> (Parallel   <$ symbol "|")
        <?> "strategy"

process :: LysParser (Parsed Process)
process =  try parProc
       <|> try seqProc
       <|> factor
       <?> "process"

factor :: LysParser (Parsed Process)
factor =  matchProc
      <|> ifProc
      <|> whileProc
      <|> forProc
      <|> exprProc
      <|> doneProc
      <|> parens process
      <?> "process"

doBlock :: LysParser (Parsed Process)
doBlock = do
    pos <- indentLevel
    keyword "do"
    indentGuard GT pos
    process

matchBlock :: LysParser (IndentOpt LysParser (Parsed Process) (Parsed Expr, Parsed Process))
matchBlock = do
    (_, s) <- spanned $ keyword "match"
    ex <- expr
    let gather cs = return $ (s <> snd (last cs) ^. ann) :< MatchProc ex cs
    return $ IndentSome Nothing gather matchCase

matchCase :: LysParser (Parsed Expr, Parsed Process)
matchCase = label "matchCase" $ do
    keyword "case"
    ex <- expr
    operator "=>"
    proc <- process
    return (ex, proc)

seqProc, parProc, conProc, matchProc, ifProc, whileProc, forProc, exprProc, doneProc :: LysParser (Parsed Process)

seqProc = label "seqProc" $ do
    (ps, s) <- spanned $ commaSep2 factor
    return $ s :< SeqProc ps

parProc = label "parProc" $ do
    (ps, s) <- spanned $ parSep2 factor
    return $ s :< ParProc ps

conProc = label "conProc" $ do
    (p, s) <- spanned factor
    operator "&"
    (q, e) <- spanned factor
    return $ (s <> e) :< ConProc p q

matchProc = label "matchProc" $ indentBlock matchBlock

ifProc = label "ifProc" $ do
    (_, s) <- spanned $ keyword "if"
    x <- expr
    keyword "then"
    tr <- process
    (fl, e) <- spanned . optional $ keyword "else" *> process
    return $ (s <> e) :< IfProc x tr fl

whileProc = label "whileProc" $ do
    (_, s) <- spanned $ keyword "while"
    x <- expr
    strat <- strategy
    (loop, e) <- spanned doBlock
    return $ (s <> e) :< WhileProc x strat loop

forProc = label "forProc" $ do
    (_, s) <- spanned $ keyword "for"
    ex <- expr
    it <- expr
    strat <- strategy
    keyword "do"
    (loop, e) <- spanned process
    return $ (s <> e) :< ForProc ex it strat loop

exprProc = label "exprProc" $ do
    (ex, s) <- lexeme $ spanned expr
    return $ s :< ExprProc ex

doneProc = label "doneProc" $ do
    (_ ,s) <- spanned $ keyword "done"
    return $ s :< DoneProc
