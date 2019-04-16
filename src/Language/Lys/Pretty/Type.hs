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
    | needsParens t = parens (prettyShow t)
    | otherwise     = prettyShow t

instance PrettyShow Type where
    prettyShow = \case
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
        AppT f ts -> prettyShow f <> anglesSep (map prettyShow ts)
        PrimT t -> prettyShow t
      where prettyShowFields = bracesSep
                . map (\ (l, t) -> string l <> ":" <+> prettyShow t)
                . Map.assocs

instance PrettyShow PrimType where
    prettyShow = \case
        IntT -> "Int"
