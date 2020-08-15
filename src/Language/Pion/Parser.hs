-- | Parser
module Language.Pion.Parser where

import Control.Monad.Except
import Data.GADT.Compare
import Data.Some
import Data.Type.Equality
import Language.Pion.Lexer.Token
import Language.Pion.Parser.Error
import qualified Text.Megaparsec as Mega

type Parser = Mega.Parsec ParseError TokenStream

lexeme :: Lexeme a -> Parser a
lexeme expectedLexeme = Mega.token matchToken expectedTokens
  where
    matchToken (Some Token {..}) = do
      Refl <- geq tokenLexeme expectedLexeme
      pure tokenData
    expectedTokens = mempty

keyword :: Keyword -> Parser ()
keyword = lexeme . Keyword

punctuation :: Punctuation -> Parser ()
punctuation = lexeme . Punctuation

primType :: PrimType -> Parser ()
primType = lexeme . PrimType

delimiter :: Delimiter -> DelimiterType -> Parser ()
delimiter delim delimType = lexeme (Delimiter delim delimType)

between :: DelimiterType -> Parser a -> Parser a
between delimType = Mega.between (delimiter Opening delimType) (delimiter Closing delimType)

identifier :: Parser Text
identifier = lexeme Identifier

integerLiteral :: Parser Integer
integerLiteral = lexeme IntegerLiteral

floatLiteral :: Parser Double
floatLiteral = lexeme FloatLiteral

charLiteral :: Parser Char
charLiteral = lexeme CharLiteral

stringLiteral :: Parser Text
stringLiteral = lexeme StringLiteral

sepBy :: Parser a -> Punctuation -> Parser [a]
sepBy p sep = p `Mega.sepBy` punctuation sep

test :: Parser [Integer]
test = between Paren $ integerLiteral `sepBy` Comma

parse :: Parser a -> String -> TokenStream -> Except ParseErrorRepr a
parse parser name source =
  liftEither
    . first reprParseErrorBundle
    $ Mega.runParser parser name source
