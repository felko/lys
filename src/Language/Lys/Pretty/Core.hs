{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Core () where

import Language.Lys.Pretty.Class
import Language.Lys.Pretty.Type
import Language.Lys.Pretty.Types
import Language.Lys.Core
import Language.Lys.Types

import qualified Data.Map as Map

instance Pretty Process where
    pretty = \case
        NilP -> "end"
        ParP p q -> pretty p <+> "|" <+> pretty q
        NewP x Nothing p -> "new" <+> string x <+> braces (pretty p)
        NewP x (Just t) p -> "new" <+> string x <> ":" <+> pretty t <+> braces (pretty p)
        OutputP x y p -> pretty x <> "!" <> parens (pretty y) <> "," <+> pretty p
        InputP x y p -> pretty x <> "?" <> parens (string y) <> "," <+> pretty p
        ReplicateP x y p -> "repeat" <+> pretty x <> "?" <> parens (string y) <> "," <+> pretty p
        SourceP x y p -> "repeat" <+> string x <> "!" <> parens (string y) <> "," <+> pretty p
        InjectP x l y p -> pretty x <> dot <> string l <> "!" <> parens (pretty y) <> "," <+> pretty p
        MatchP x bs -> "match" <+> pretty x <+> bracesSep (map pretty bs)
        CallP p xs -> string p <> parensSep (map pretty xs)
        ContractP x y p q -> "contract" <+> string x <> parens (string y) <+> braces (pretty p) <+> "|" <+> pretty q
