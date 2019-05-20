{-# LANGUAGE OverloadedStrings #-}

module Language.Lys.Pretty.Type () where

import Language.Lys.Pretty.Class
import Language.Lys.Types

import qualified Data.Map as Map

isUnaryTypeOp, isBinaryTypeOp :: Type -> Bool
isUnaryTypeOp = \case
    DualT{} -> True
    OfCourseT{} -> True
    WhyNotT{} -> True
    _ -> False

isBinaryTypeOp = \case
    ParT{} -> True
    TensorT{} -> True
    _ -> False

subUnOpTExpr, subBinOpTExpr :: Type -> Doc
subUnOpTExpr t
    | isBinaryTypeOp t || isUnaryTypeOp t = parens (pretty t)
    | otherwise = pretty t

subBinOpTExpr t
    | isBinaryTypeOp t = parens (pretty t)
    | otherwise = pretty t

instance Pretty Type where
    pretty = \case
        TopT -> blue "⊤"
        BottomT -> blue "⊥"
        OneT -> red "1"
        ZeroT -> red "0"
        OfCourseT t -> red $ "!" <> subUnOpTExpr t
        WhyNotT t -> blue $ "?" <> subUnOpTExpr t
        TensorT a b -> blue $ subBinOpTExpr a <> ";" <+> subBinOpTExpr b
        ParT a b -> blue $ subBinOpTExpr a <+> "|" <+> subBinOpTExpr b
        PlusT fs -> red $ "+" <> prettyShowFields fs
        WithT fs -> blue $ "&" <> prettyShowFields fs
        DualT t -> magenta $ "~" <> subUnOpTExpr t
        VarT n -> dullwhite (string n)
        RigidT n -> white (string n)
        IdentT n -> white (string n)
        AppT f ts -> pretty f <> anglesSep (map pretty ts)
        PrimT t -> pretty t
      where prettyShowFields = bracesSep
                . map (\ (l, t) -> yellow (string l) <> ":" <+> pretty t)
                . Map.assocs

instance Pretty PrimType where
    pretty = \case
        IntT -> "Int"
