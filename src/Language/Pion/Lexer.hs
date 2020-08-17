-- | Lexer
module Language.Pion.Lexer where

import Control.Monad.Except (MonadError)
import Language.Pion.Lexer.Error (LexerErrorRepr)
import Language.Pion.Lexer.Monad (runLexer)
import Language.Pion.Lexer.Token (Stream)
import Language.Pion.Lexer.Tokenize (tokenize)

lex :: MonadError LexerErrorRepr m => String -> LText -> m Stream
lex = runLexer tokenize
