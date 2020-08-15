module Main where

import Control.Monad.Except (runExcept)
import Language.Pion.Lexer (lex)
import Language.Pion.Lexer.Token (TokenStream (..))
import Language.Pion.Parser (parseExpression)
import System.Environment (getArgs)

main :: IO ()
main = do
  [s] <- getArgs
  let sourceName = "(input)"
  result <- runExceptT do
    stream <- lex sourceName (toLText s)
    liftIO $ print (streamTokens stream)
    parseExpression sourceName stream
  print result
