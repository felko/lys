-- | Process parser.
module Language.Pion.Parser.Process
  ( process,
  )
where

import Data.Some
import Language.Pion.Lexer.Token
import Language.Pion.Name
import Language.Pion.Parser.Error
import Language.Pion.Parser.Expr
import Language.Pion.Parser.Monad
import Language.Pion.Syntax.Process

-- | Parse an process.
process :: Parser Process
process = undefined

action :: Parser (Some Action)
action = undefined
