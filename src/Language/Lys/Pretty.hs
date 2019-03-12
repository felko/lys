{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  #-}

module Language.Lys.Pretty where

import Language.Lys.Types

import Data.Maybe (maybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

class PrettyShow a where
    prettyShow :: a -> Doc

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint = putDoc . (<> "\n") . prettyShow

instance PrettyShow Type where
    prettyShow = \case
        IntT          -> "Int"
        FloatT        -> "Float"
        CharT         -> "Char"
        StringT       -> "String"
        IdentT n      -> text n
        VarT n        -> text n
        QuoteT s      -> backticks (prettyShow s)
        RecordT r     -> let (fields, ext)  = recordToList r
                             formattedElems = map (\ (f, t) -> text f <> ": " <> prettyShow t) fields ++ maybe [] (\ n -> [text n <> "..."]) ext
                         in braces . cat . punctuate (string ", ") $ formattedElems
        VariantT r    -> let (fields, ext)  = recordToList r
                             formattedElems = map (\ (f, t) -> text f <> ": " <> prettyShow t) fields ++ maybe [] (\ n -> [text n <> "..."]) ext
                         in braces . cat . punctuate (string " | ") $ formattedElems
        EmptyT        -> "{}"
        --ExtendT l t r -> braces (text l <> ":" <+> prettyShow t <+> "|" <+> prettyShow r)

instance PrettyShow Process where
    prettyShow = \case
        InputP x y p -> prettyShow x <> parens (text y) <> "," <+> prettyShow p
        OutputP x y  -> prettyShow x <> brackets (prettyShow x)
        NewP x t p   -> "new " <> text x <> ":" <+> prettyShow t <> braces (prettyShow p)
        NewPI x p    -> "new " <> text x <+> braces (prettyShow p)
        ParP p q     -> prettyShow p <+> "|" <+> prettyShow q
        ProcP x t p  -> "proc" <> parens (text x <> ":" <+> prettyShow t) <+> "->" <+> prettyShow p
        ProcPI x p   -> "proc" <> parens (text x) <+> "->" <+> prettyShow p
        CallP p x    -> prettyShow p <+> prettyShow x
        DropP x      -> "$" <> prettyShow x
        VarP n       -> text n
        NilP         -> "0"

instance PrettyShow Session where
    prettyShow = \case
        ReadS x s   -> prettyShow x <> "?, " <> align (prettyShow s)
        WriteS x    -> prettyShow x <> "!"
        s@(ProcS{}) -> let (params, s')     = uncurrySession s
                           showParam (x, t) = text x <> ":" <+> prettyShow t
                       in "proc" <> tupled (map showParam params) <+> "->" <+> align (prettyShow s')
        ParS s s'   -> prettyShow s <+> "|" <+> prettyShow s'
        NilS        -> "0"
        VarS n      -> text n

uncurrySession :: Session -> ([(String, Type)], Session)
uncurrySession (ProcS x t s) = ((x, t) : params, s')
    where (params, s') = uncurrySession s
uncurrySession s = ([], s)

instance PrettyShow Name where
    prettyShow = \case
        LitN l     -> prettyShow l
        FieldN x f -> prettyShow x <> "." <> text f
        RecN fs    -> braces . cat . punctuate comma $ map (\ (f, t) -> text f <> ":" <+> prettyShow t) fs
        CaseN x f  -> braces . cat $ punctuate " | " [prettyShow x <> ":" <+> text f, "..."]
        QuoteN p   -> backticks (prettyShow p)
        VarN n     -> text n

instance PrettyShow Literal where
    prettyShow = \case
        IntL    x -> integer x
        FloatL  x -> float x
        CharL   c -> squotes (char c)
        StringL s -> dquotes (string s)

backticks :: Doc -> Doc
backticks = enclose (char '`') (char '`')