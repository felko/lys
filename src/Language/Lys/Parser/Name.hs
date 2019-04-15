{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Name where

import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Prelude hiding (replicate)

import Control.Applicative
import Control.Monad

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

name, varN, litN :: Parser Name
name = varN <|> litN <?> "name"

varN = VarN <$> identifier
litN = LitN <$> literal

literal, intL, oneL :: Parser Literal
literal = oneL <|> intL <?> "literal"

intL = IntL <$> integerLiteral
oneL = OneL <$ symbol "()"
