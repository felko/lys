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

instance PrettyShow Process where
    prettyShow = \case
        NilP -> "end"
        ParP p q -> prettyShow p <+> "|" <+> prettyShow q
        NewP x Nothing p -> "new" <+> string x <+> braces (prettyShow p)
        NewP x (Just t) p -> "new" <+> string x <> ":" <+> prettyShow t <+> braces (prettyShow p)
        OutputP x y p -> prettyShow x <> "!" <> parens (prettyShow y) <> "," <+> prettyShow p
        InputP x y p -> prettyShow x <> "?" <> parens (string y) <> "," <+> prettyShow p
        ReplicateP x y p -> "repeat" <+> prettyShow x <> "?" <> parens (string y) <> "," <+> prettyShow p
        SourceP x y p -> "repeat" <+> string x <> "!" <> parens (string y) <> "," <+> prettyShow p
        InjectP x l y p -> prettyShow x <> dot <> string l <> "!" <> parens (prettyShow y) <> "," <+> prettyShow p
        MatchP x bs -> "match" <+> prettyShow x <+> bracesSep (map prettyShow bs)
        CallP p xs -> string p <> parensSep (map prettyShow xs)
        ContractP x y p q -> "contract" <+> string x <> parens (string y) <+> braces (prettyShow p) <+> "|" <+> prettyShow q

instance PrettyShow Constraint where
    prettyShow = \case
        NoneC -> "()"

instance PrettyShow TypeParam where
    prettyShow = \case
        ParamTP t    -> string t
        ConstrTP t c -> string t <> ":" <+> prettyShow c

instance PrettyShow NameParam where
    prettyShow = \case
        InferredNP  x   -> string x
        AnnotatedNP x t -> string x <> ":" <+> prettyShow t

instance PrettyShow Declaration where
    prettyShow = \case
        TypeD n [] t -> "type" <+> string n <+> "=" <+> prettyShow t
        TypeD n as t -> "type" <+> string n <> anglesSep (map prettyShow as) <+> "=" <+> prettyShow t
        ProcessD n [] ps p -> "process" <+> string n <> parensSep (map prettyShow ps) <+> braces (prettyShow p)
        ProcessD n as ps p -> "process" <+> string n <> anglesSep (map prettyShow as) <+> parensSep (map prettyShow ps) <+> bracesBlock (prettyShow p)

instance PrettyShow Export where
    prettyShow = \case
        TypeE t -> string t
        ProcessE p -> string p
        ModuleE mod -> dotSep (map string mod)

instance PrettyShow Import where
    prettyShow = \case
        ImportI mod -> dotSep (map string mod)

instance PrettyShow Program where
    prettyShow (Program mod exps imps decls) = vsep
        [ "module" <+> dotSep (map string mod) <+> parensSep (map prettyShow exps)
        , line
        , vsep (map prettyShow imps)
        , line
        , vsep (map ((<> line) . prettyShow) decls) ]
