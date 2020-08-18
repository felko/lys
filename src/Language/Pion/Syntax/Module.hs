-- | Syntax of modules.
module Language.Pion.Syntax.Module
  ( Module (..),
  )
where

import Language.Pion.Pretty
import Language.Pion.SourceSpan
import Language.Pion.Syntax.Declaration (Declaration)

data Module = Module
  { moduleDecls :: [Located Declaration]
  }
  deriving (Eq, Show)

instance Pretty Module where
  pretty Module {..} =
    prettyASTList (pretty <$> moduleDecls)
