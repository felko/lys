{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Types () where

import Language.Lys.Pretty.Class
import Language.Lys.Pretty.Type
import Language.Lys.Types
import Language.Lys.Types.Env
import Language.Lys.Types.Context

import qualified Data.Map as Map

instance Pretty Name where
    pretty = \case
        VarN x -> string x
        LitN l -> pretty l

instance Pretty Literal where
    pretty = \case
        OneL -> "()"
        IntL x -> integer x

instance Pretty a => Pretty (Branch a) where
    pretty (Branch l pat p) = case pat of
        WildcardPat -> string l <> ":" <+> pretty p
        VarPat y    -> string l <> parens (string y) <> ":" <+> pretty p

instance Pretty a => Pretty (Env a) where
    pretty (Env m)
        | Map.null m = "•"
        | otherwise = cat $ punctuate ", " (map (\ (x, t) -> string x <> ": " <> pretty t) (Map.assocs m))

instance Pretty Context where
    pretty (Context d t) = parens ("∆ = " <> pretty d <> "; Θ = " <> pretty t)
