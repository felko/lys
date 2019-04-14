module Language.Lys.TypeChecking.Debug where

import Language.Lys.TypeChecking.Types
import Language.Lys.Pretty

import Control.Monad.Except

import Control.Lens

import Debug.Trace

indentDebug :: (String -> Infer a) -> Infer a
indentDebug f = do
    d <- use depth
    depth += 1
    r <- f (replicate d '\t')
    depth -= 1
    pure r

rule :: (Show a, Show b) => Judgement b -> String -> Infer a -> Infer a
rule (p :⊢ ctx) name i = indentDebug \ tab ->
    trace (tab ++ name ++ " " ++ show p ++ "\n\t" ++ tab ++ show ctx) $
        (i >>= \ x -> trace (tab ++ '/':name ++ " " ++ show x) $ pure x)
            `catchError` \ e -> trace (tab ++ '/':name ++ ": " ++ show e) (throwError e)

unifying :: (Show a, Unifiable a a) => a -> a -> Infer (Subst a) -> Infer (Subst a)
unifying t t' = indentDebug . flip \ tab -> trace (tab <> show t <> " ~ " <> show t')
