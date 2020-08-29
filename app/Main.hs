module Main where

import Control.Monad.Except
import Language.Pion.Lexer
import Language.Pion.Pass.Parsing
import Prettyprinter (pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  source <- readFileLText path
  let result =
        runExcept $
          lex path source >>= parseModule path
  case result of
    Right ast -> putDoc (pretty ast)
    Left err -> putDoc err
  putStrLn ""
