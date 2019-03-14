{-# LANGUAGE
    LambdaCase
  , ViewPatterns
  , OverloadedStrings
  #-}

module Language.Lys.Pretty where

import Language.Lys.Types

import Data.Maybe (maybe)
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

class PrettyShow a where
    prettyShow :: a -> Doc

prettyPrint :: PrettyShow a => a -> IO ()
prettyPrint = putStrLn . flip displayS "" . renderSmart 1.0 80 . (<> "\n") . prettyShow

instance PrettyShow Type where
    prettyShow = \case
        IntT -> "Int"
        FloatT -> "Float"
        CharT -> "Char"
        StringT -> "String"
        IdentT n -> text n
        VarT n -> text n
        QuoteT s -> backticks (prettyShow s)
        RecordT r -> prettyShow r

instance PrettyShow RecordType where
    prettyShow = \case
        r@SumRT{}  -> sepWith ", " r
        r@ProdRT{} -> sepWith " | " r
        EmptyRT    -> "{}"
        VarRT n    -> text n
      where formattedFields (accumulateFields -> (fs, ext)) =
                map (\ (Field f t) -> text f <> ": " <> prettyShow t) fs
                    ++ maybe [] (\ n -> [text n <> "..."]) ext
            sepWith sep = braces . cat . punctuate sep . formattedFields

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
        RecN r     -> prettyShow r
        QuoteN p   -> backticks (prettyShow p)
        VarN n     -> text n

instance PrettyShow Record where
    prettyShow = \case
        SumR l x -> braces (string l <> ":" <+> prettyShow x)
        ProdR fs -> braces . cat . punctuate comma $ map (\ (f, x) -> text f <> ":" <+> prettyShow x) fs

instance PrettyShow Literal where
    prettyShow = \case
        IntL    x -> integer x
        FloatL  x -> float x
        CharL   c -> squotes (char c)
        StringL s -> dquotes (string s)

backticks :: Doc -> Doc
backticks = enclose (char '`') (char '`')
