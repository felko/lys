-- | Common utility functions for pretty printing.
module Language.Pion.Pretty
  ( -- * Reexports
    module Prettyprinter,
    module Prettyprinter.Render.Terminal,

    -- * Conversion
    docToString,

    -- * Literals
    prettyCharLiteral,
    prettyStringLiteral,

    -- * AST pretty printing
    prettyLabelled,
    prettyField,
    prettyASTNode,
    prettyASTList,
  )
where

import Data.Char (showLitChar)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prelude

docToString :: Doc AnsiStyle -> String
docToString =
  toString
    . renderStrict
    . layoutPretty defaultLayoutOptions

-- | Pretty print an escaped character literal.
prettyCharLiteral :: Char -> Doc ann
prettyCharLiteral char = squotes (pretty (showLitChar char ""))

-- | Pretty print a string literal, surrounded by double quotes.
-- Escapes characters as necessary.
prettyStringLiteral :: Text -> Doc ann
prettyStringLiteral str = pretty (show @Text str)

-- | Print a label in front of a pretty printed item.
prettyLabelled :: Doc ann -> Doc ann -> Doc ann
prettyLabelled label labelled = group $ label <> labelled

-- | Pretty print an AST field.
prettyField :: Doc ann -> Doc ann -> Doc ann
prettyField name value = name <> colon <+> value

-- | Pretty print an AST node, given its label and fields.
prettyASTNode :: Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
prettyASTNode label fields =
  prettyLabelled label $
    nest 2 (line <> align (encloseSep open close separator prettyFields))
  where
    open = flatAlt "" "{ "
    close = flatAlt "" " }"
    separator = flatAlt "" ", "
    prettyFields = fmap (uncurry prettyField) fields

-- | Pretty print a bulleted list.
prettyASTList :: [Doc ann] -> Doc ann
prettyASTList elements =
  group $ nest 2 (line <> align (encloseSep open close separator bulletedList))
  where
    open = flatAlt "" "["
    close = flatAlt "" " ]"
    bullet = flatAlt "- " ""
    separator = flatAlt "" ", "
    bulletedList = fmap (bullet <>) elements
