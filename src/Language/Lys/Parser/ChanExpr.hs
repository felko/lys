{-# LANGUAGE
    OverloadedStrings
  #-}

module Language.Lys.Parser.Rule.ChanExpr
    ( capability
    , chanExpr
    , factorCExpr, termCExpr
    , varCExpr, appCExpr, procCExpr, recCExpr, altCExpr
    ) where

import Language.Lys.Parser.Tree.ChanExpr
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Type
import Language.Lys.Type.Name

import Control.Applicative

import Control.Comonad.Cofree(Cofree((:<)))

import qualified Text.Megaparsec as Mega

import Debug.Trace

capability :: LysParser Capability
capability =  (Read  <$ operator "?")
          <|> (Write <$ operator "!")
          <|> (Ref   <$ operator "@")
          <?> "capability"

withCap :: LysParser a -> LysParser (a, Capability)
withCap p = flip (,) <$> capability <*> p

chanExpr :: LysParser (Parsed ChanExpr)
chanExpr =  try recCExpr
        <|> try altCExpr
        <|> factorCExpr
        <?> "channelExpr"

factorCExpr, termCExpr :: LysParser (Parsed ChanExpr)
factorCExpr =  try procCExpr
           <|> termCExpr
           <?> "factorCExpr"

termCExpr =  varCExpr
         <|> appCExpr
         <|> parens chanExpr
         <?> "termCExpr"

varCExpr, appCExpr, procCExpr, recCExpr, altCExpr :: LysParser (Parsed ChanExpr)

varCExpr = label "varCExpr" $ do
    ((n, c), s) <- spanned (withCap upperIdentifier)
    return $ s :< VarCExpr c n

appCExpr = label "appCExpr" $ do
    ((((n, ns), cs), c), s) <- spanned
                        . withCap
                        . parens $ (,) <$> spanned upperIdentifier
                                       <*> some termCExpr
    return $ s :< AppCExpr (ns :< VarCExpr c n) cs

procCExpr = label "procCExpr" $ do
    pos <- indentLevel
    spanCofree $ ProcCExpr <$> some ( (indentGuard GT pos <|> indentGuard EQ pos)
                                    *> lexeme termCExpr )

recCExpr = label "recCExpr" . spanCofree $ RecCExpr <$> commaSep1 field

altCExpr = label "altCExpr" . spanCofree $ AltCExpr <$> parSep1 constr

field, constr :: LysParser (Name, Parsed ChanExpr)
field = label "field" $ do
    n <- lexeme identifier
    operator ":"
    c <- lexeme factorCExpr
    return (n, c)

constr = label "constr" $ do
    n <- lexeme upperIdentifier
    operator ":"
    c <- lexeme factorCExpr
    return (n, c)
