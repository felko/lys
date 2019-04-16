{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Type () where

import Language.Lys.Pretty.Class
import Language.Lys.Types

import qualified Data.Map as Map

needsParens :: Type -> Bool
needsParens = \case
    TopT -> False
    BottomT -> False
    OneT -> False
    ZeroT -> False
    VarT{} -> False
    IdentT{} -> False
    PrimT{} -> False
    AppT{} -> False
    _ -> True

subTypeExpr :: Type -> Doc
subTypeExpr t
    | needsParens t = parens (pretty t)
    | otherwise     = pretty t

instance Pretty Type where
    pretty = \case
        TopT -> "⊤"
        BottomT -> "⊥"
        OneT -> "1"
        ZeroT -> "0"
        OfCourseT t -> "!" <> subTypeExpr t
        WhyNotT t -> "?" <> subTypeExpr t
        TensorT a b -> subTypeExpr a <> ";" <+> subTypeExpr b
        ParT a b -> subTypeExpr a <+> "|" <+> subTypeExpr b
        PlusT fs -> "+" <> prettyShowFields fs
        WithT fs -> "&" <> prettyShowFields fs
        DualT t -> "~" <> subTypeExpr t
        VarT n -> string n
        IdentT n -> string n
        AppT f ts -> pretty f <> anglesSep (map pretty ts)
        PrimT t -> pretty t
      where prettyShowFields = bracesSep
                . map (\ (l, t) -> string l <> ":" <+> pretty t)
                . Map.assocs

instance Pretty PrimType where
    pretty = \case
        IntT -> "Int"
