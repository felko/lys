-- | Lexer
module Language.Pion.Lexer where

import Control.Monad.Except
import Language.Pion.Lexer.Error (LexerErrorRepr, reprLexerErrorBundle)
import Language.Pion.Lexer.Token
import Language.Pion.Lexer.Tokenize (tokenize)
import qualified Text.Megaparsec as Mega

lex :: String -> LText -> Except LexerErrorRepr TokenStream
lex name source =
  liftEither
    . first reprLexerErrorBundle
    $ Mega.runParser tokenize name source
