-- | Lexer
module Language.Pion.Lexer
  ( Lexer,
    runLexer,
    LexerErrorBundle,
    LexerErrorRepr,
    lex,
    tokenize,
  )
where

import Control.Monad.Except
import Language.Pion.Lexer.Error
import Language.Pion.Lexer.Monad
import Language.Pion.Lexer.Token
import Language.Pion.Lexer.Tokenize

lex ::
  Monad m =>
  String ->
  LText ->
  ExceptT LexerErrorRepr m Stream
lex name source =
  withExceptT reprLexerErrorBundle $
    runLexer tokenize name source
