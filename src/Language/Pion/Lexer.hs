-- | Lexer
module Language.Pion.Lexer where

import Control.Monad.Except
import Language.Pion.Lexer.Error (LexerError (..), tokenErrorBundleToLexerError)
import Language.Pion.Lexer.Token (Token (..))
import Language.Pion.Lexer.Tokenize (tokenize)
import qualified Text.Megaparsec as Mega

lex :: LText -> String -> Except LexerError [Token]
lex source name =
  liftEither
    . first tokenErrorBundleToLexerError
    $ Mega.runParser tokenize name source
