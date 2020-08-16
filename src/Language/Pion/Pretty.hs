-- | Common utility functions for pretty printing.
module Language.Pion.Pretty
  ( module Prettyprinter,
    prettyCharLiteral,
    prettyStringLiteral,
    prettyASTNode,
    prettyField,
  )
where

import Data.Char (showLitChar)
import Prettyprinter
import Prelude

prettyCharLiteral :: Char -> Doc ann
prettyCharLiteral char = squotes (pretty (showLitChar char ""))

prettyStringLiteral :: Text -> Doc ann
prettyStringLiteral str = pretty (show @Text @Text str)

prettyASTNode :: Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
prettyASTNode label fields =
  group $
    label <> nest 2 (line <> align (encloseSep open close separator prettyFields))
  where
    open = flatAlt "" "{ "
    close = flatAlt "" " }"
    separator   = flatAlt "" ", "
    prettyFields = fmap (uncurry prettyField) fields

prettyField :: Doc ann -> Doc ann -> Doc ann
prettyField name value = name <> colon <+> value
