-- | Parser for an entire module.
module Language.Pion.Parser.Module
  ( module',
  )
where

import Language.Pion.Parser.Declaration (declaration)
import Language.Pion.Parser.Monad
import Language.Pion.Syntax.Concrete.Module

module' :: Parser Module
module' = Module <$> many declaration
