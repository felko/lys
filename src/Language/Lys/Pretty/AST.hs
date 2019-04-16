{-# LANGUAGE
    OverloadedStrings
  #-}

module Language.Lys.Pretty.AST where

import Language.Lys.Pretty.Class
import Language.Lys.Pretty.Type
import Language.Lys.Pretty.Types
import Language.Lys.Parser.AST
import Language.Lys.Types

import Control.Lens hiding (Context(..))

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
        MatchP x bs -> "match" <+> pretty x <+> bracesParSep (map pretty bs)
        CallP p xs -> string p <> parensSep (map pretty xs)
        ContractP x y p q -> "contract" <+> string x <> parens (string y) <+> braces (pretty p) <+> "|" <+> pretty q

instance Pretty Constraint where
    pretty = \case
        NoneC -> "()"

instance Pretty TypeParam where
    pretty = \case
        ParamTP t    -> string t
        ConstrTP t c -> string t <> ":" <+> pretty c

instance Pretty NameParam where
    pretty = \case
        InferredNP  x   -> string x
        AnnotatedNP x t -> string x <> ":" <+> pretty t

instance Pretty Declaration where
    pretty = \case
        TypeD n [] t -> "type" <+> string n <+> "=" <+> pretty t
        TypeD n as t -> "type" <+> string n <> anglesSep (map pretty as) <+> "=" <+> pretty t
        ProcessD n [] ps p -> "process" <+> string n <> parensSep (map pretty ps) <+> braces (pretty p)
        ProcessD n as ps p -> "process" <+> string n <> anglesSep (map pretty as) <+> parensSep (map pretty ps) <+> bracesBlock (pretty p)

instance Pretty Export where
    pretty = \case
        TypeE t -> string t
        ProcessE p -> string p
        ModuleE mod -> dotSep (map string mod)

instance Pretty Import where
    pretty = \case
        ImportI mod -> dotSep (map string mod)

instance Pretty Program where
    pretty (Program mod exps imps decls) = vsep
        [ "module" <+> dotSep (map string mod) <+> parensSep (map pretty exps)
        , line
        , vsep (map pretty imps)
        , line
        , vsep (map ((<> line) . pretty) decls) ]
