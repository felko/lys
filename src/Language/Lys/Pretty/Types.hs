{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Types () where

import Language.Lys.Pretty.Class
import Language.Lys.Pretty.Type
import Language.Lys.Types
import Language.Lys.Types.Env
import Language.Lys.Types.Context

import qualified Data.Map as Map

instance PrettyShow Name where
    prettyShow = \case
        VarN x -> string x
        LitN l -> prettyShow l

instance PrettyShow Literal where
    prettyShow = \case
        OneL -> "()"
        IntL x -> integer x

instance PrettyShow a => PrettyShow (Branch a) where
    prettyShow (Branch l pat p) = case pat of
        WildcardPat -> string l <> ":" <+> prettyShow p
        VarPat y    -> string l <> parens (string y) <> ":" <+> prettyShow p

instance PrettyShow a => PrettyShow (Env a) where
    prettyShow (Env m)
        | Map.null m = "•"
        | otherwise = cat $ punctuate ", " (map (\ (x, t) -> string x <> ": " <> prettyShow t) (Map.assocs m))

instance PrettyShow Context where
    prettyShow (Context d t) = parens ("∆ = " <> prettyShow d <> "; Θ = " <> prettyShow t)
