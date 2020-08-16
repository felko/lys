-- | Pattern parser.
module Language.Pion.Parser.Pattern
  ( pattern',
  )
where

import Language.Pion.Lexer.Token
import Language.Pion.Name
import Language.Pion.Parser.Error
import Language.Pion.Parser.Monad
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Pattern

pattern' :: Parser Pattern
pattern' = undefined
