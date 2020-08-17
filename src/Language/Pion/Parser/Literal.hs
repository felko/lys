-- | Parser for literal values.
module Language.Pion.Parser.Literal (literal) where

import Language.Pion.Parser.Monad
import Language.Pion.Syntax.Literal

-- | Parse a literal value.
literal :: Parser Literal
literal =
  (IntegerLiteral <$> integerLiteral)
    <|> (FloatLiteral <$> floatLiteral)
    <|> (CharLiteral <$> charLiteral)
    <|> (StringLiteral <$> stringLiteral)
    <?> "literal"
