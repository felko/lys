{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Type where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative
import Control.Monad

import qualified Data.Map as Map

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega

type' :: Parser Type
type' =  try tensorT
     <|> try parT
     <|> plusT <|> withT
     <|> binaryFactor
     <?> "type"

appFactor, unaryFactor, binaryFactor :: Parser Type

appFactor =  topT <|> bottomT
         <|> oneT <|> zeroT
         <|> varT <|> identT
         <?> "type"
         
unaryFactor = try appT <|> appFactor <?> "type"

binaryFactor =  ofCourseT
            <|> whyNotT
            <|> dualT
            <|> unaryFactor
            <?> "type"

varT, identT, appT :: Parser Type
varT = VarT <$> identifier
identT = IdentT <$> upperIdentifier
appT = AppT <$> appFactor <*> angles (commaSep1 type')

topT, bottomT, oneT, zeroT :: Parser Type
topT = TopT <$ symbol "~0"
bottomT = BottomT <$ symbol "~1"
oneT = OneT <$ symbol "1"
zeroT = OneT <$ symbol "0"

ofCourseT, whyNotT, dualT :: Parser Type
ofCourseT = symbol "!" >> OfCourseT <$> unaryFactor
whyNotT = symbol "?" >> WhyNotT <$> unaryFactor
dualT = symbol "~" >> DualT <$> unaryFactor

tensorT, parT :: Parser Type
tensorT = foldr1 TensorT <$> binaryFactor `Mega.sepBy1` symbol "*"
parT = foldr1 ParT <$> binaryFactor `Mega.sepBy1` symbol "|"

plusT, withT :: Parser Type
plusT = do
    symbol "+"
    fs <- braces (commaSep1 (field OneT))
    pure (PlusT (Map.fromList fs))
withT = do
    symbol "&"
    fs <- braces (commaSep1 (field BottomT))
    pure (WithT (Map.fromList fs))

field :: Type -> Parser (String, Type)
field d = do
    f <- identifier
    t <- (symbol ":" *> type') <|> pure d
    pure (f, t)
