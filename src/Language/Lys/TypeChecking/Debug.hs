module Language.Lys.TypeChecking.Debug where

import Language.Lys.TypeChecking.Types
import Language.Lys.Pretty

import Control.Monad.Except

import Control.Lens

import Debug.Trace

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

indentDebug :: (String -> Infer a) -> Infer a
indentDebug f = do
    d <- use depth
    depth += 1
    r <- f (replicate d '\t')
    depth -= 1
    pure r

rule :: (Pretty a, Pretty b) => Judgement b -> String -> Infer a -> Infer a
rule (p :⊢ ctx) name i = indentDebug \ tab ->
    trace (tab ++ name ++ " " ++ prettyShow p ++ "\n\t" ++ tab ++ prettyShow ctx) $
        (i >>= \ x -> trace (tab ++ '/':name ++ " " ++ prettyShow x) $ pure x)
            `catchError` \ es -> trace (tab ++ '/':name ++ ": " ++ prettyShow es) (throwError es)

unifying :: (Pretty a, Eq a, Unifiable a a) => ((a, a) -> Infer b) -> a -> a -> Infer b
unifying u t t' = indentDebug \ tab -> if t == t' then
        u (t, t')
    else
        trace (tab <> prettyShow t <> " ~ " <> prettyShow t') $ u (t, t')
