module Main where

import Control.Monad.Except (runExcept)
import Language.Pion.Lexer (lex)
import Language.Pion.Lexer.Token (TokenStream (..))
import Language.Pion.Parser (parseExpression)
import Prettyprinter (pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Environment (getArgs)

main :: IO ()
main = do
  [s] <- getArgs
  let sourceName = "(input)"
  result <- runExceptT do
    stream <- lex sourceName (toLText s)
    liftIO $ putDoc (pretty stream)
    parseExpression sourceName stream
  putStrLn ""
  case result of
    Right ast -> putDoc (pretty ast)
    Left err -> print err
  putStrLn ""
