module Main where

import Control.Monad.Except (runExceptT)
import Language.Pion.Lexer
import Language.Pion.Parser
import Prettyprinter (pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  source <- readFileLText path
  result <- runExceptT do
    stream <- lex path source
    liftIO $ putDoc (pretty stream)
    parseProcess path stream
  putStrLn ""
  case result of
    Right ast -> putDoc (pretty ast)
    Left err -> putDoc err
  putStrLn ""
