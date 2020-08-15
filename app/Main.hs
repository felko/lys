module Main where

import Control.Monad.Except
import Language.Pion.Lexer
import Language.Pion.Parser
import System.Environment
import qualified Text.Megaparsec as Mega

main :: IO ()
main = do
  [s] <- getArgs
  let result = runExcept do
        tokens <- lex "(input)" $ toLText s
        parse test "(input)" tokens
  print result
