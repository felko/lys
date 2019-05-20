{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Class
    ( PP.Pretty(..)
    , prettyShow
    , prettyPrint
    , module PP
    , parensSep, bracesSep, bracesParSep, anglesSep
    , dotSep
    , bracesBlock
    ) where

import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

prettyShow :: Pretty a => a -> String
prettyShow = filter (/= '\n') . flip displayS "" . renderPretty 0.8 maxBound . pretty

prettyPrint :: Pretty a => a -> IO ()
prettyPrint x = putDoc (pretty x) >> putStrLn ""

parensSep, bracesSep, bracesParSep, anglesSep :: [Doc] -> Doc
parensSep    = encloseSep "(" ")" ", "
bracesSep    = encloseSep "{ " " }" ", "
bracesParSep = encloseSep "{ " " }" " | "
anglesSep    = encloseSep "<" ">" ", "

dotSep :: [Doc] -> Doc
dotSep = cat . punctuate "."

bracesBlock :: Doc -> Doc
bracesBlock d = "{" <> line <> nest 4 d <> line <> "}"
