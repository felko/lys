module Language.Lys.Parser.Rule.Literal
    ( literal
    ) where

import Language.Lys.Parser.Tree.Literal
import Language.Lys.Parser.Monad
import Language.Lys.Parser.Lexer
import Language.Lys.Parser.Type

import Text.Megaparsec as Mega

literal :: LysParser Literal
literal =  Mega.try floatLit
       <|> intLit
       <|> charLit
       <|> stringLit
       <?> "literal"

intLit, floatLit, charLit, stringLit :: LysParser Literal
intLit    = IntLit    <$> integerLiteral
floatLit  = FloatLit  <$> floatLiteral
charLit   = CharLit   <$> charLiteral
stringLit = StringLit <$> stringLiteral