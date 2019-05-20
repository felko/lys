{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Parser.Type where

import Language.Lys.Parser.AST
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Types
import Language.Lys.Types

import Control.Applicative
import Control.Monad

import Control.Lens hiding (Context)

import Data.Bool (bool)
import qualified Data.Map as Map

import Control.Monad.Combinators.Expr

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega


unary sym f = Prefix (f <$ symbol sym)
binary sym f = InfixR (f <$ symbol sym)

typeOperators :: [[Operator Parser Type]]
typeOperators =
    [ [ unary "?" WhyNotT, unary "!" OfCourseT, unary "~" DualT]
    , [ binary ";" TensorT, binary "|" ParT ]
    , [ binary "->" implication
      , binary "-o" lollipop ]
    ]

lollipop, implication :: Type -> Type -> Type
lollipop a b = ParT (DualT a) b
implication a b = lollipop (OfCourseT a) b

type' :: Parser Type
type' = makeExprParser (lexeme factor) typeOperators

appFactor, factor :: Parser Type

appFactor =  topT <|> bottomT
         <|> oneT <|> zeroT
         <|> identT
         <|> plusT <|> withT
         <|> parens type'
         <?> "type"
         
factor = try appT <|> appFactor <?> "type"

identT, appT :: Parser Type
identT = do
    i <- upperIdentifier
    ($ i) . bool IdentT RigidT <$> isRigid i
appT = AppT <$> appFactor <*> angles (commaSep1 type')

topT, bottomT, oneT, zeroT :: Parser Type
topT = TopT <$ symbol "⊤"
bottomT = BottomT <$ symbol "⊥"
oneT = OneT <$ symbol "1"
zeroT = OneT <$ symbol "0"

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
