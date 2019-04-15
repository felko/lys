{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Class
    ( PrettyShow(..)
    , prettyPrint
    , module PP
    , parensSep, bracesSep, anglesSep
    , dotSep
    , bracesBlock
    ) where

import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

class PrettyShow a where
    prettyShow :: a -> Doc

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint x = putDoc (prettyShow x) >> putStrLn ""

parensSep, bracesSep, anglesSep :: [Doc] -> Doc
parensSep = encloseSep "(" ")" ", "
bracesSep = encloseSep "{ " " }" ", "
anglesSep = encloseSep "<" ">" ", "

dotSep :: [Doc] -> Doc
dotSep = cat . punctuate "."

bracesBlock :: Doc -> Doc
bracesBlock d = "{" <> line <> nest 4 d <> line <> "}"
