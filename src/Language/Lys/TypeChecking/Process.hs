{-# LANGUAGE
    LambdaCase
  , FlexibleInstances
  , TypeFamilies
  , BlockArguments
  , OverloadedStrings
  , MultiParamTypeClasses
  , TypeApplications
  , ViewPatterns
  #-}

module Language.Lys.TypeChecking.Process where

import Language.Lys.TypeChecking.Types
import Language.Lys.Types

import Control.Arrow ((>>>))
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Functor.Compose

import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Control.Lens hiding (Context)

import Debug.Trace

data Binding = Binding
    { _bindingName :: String
    , _bindingType :: Type }
    deriving (Eq)

instance Ord Binding where
    compare = compare `on` _bindingName

normalForm :: Process -> Process
normalForm = \case
    -- S0
    ParP NilP p -> normalForm p
    ParP p NilP -> normalForm p

    -- (Sν0)
    p@NewP{} -> case q of
            NilP -> NilP
            _ -> foldr (uncurry NewP) q (Set.toAscList names)
        where (names, q) = accumNames p Set.empty
              accumNames (NewP x t (normalForm -> q@NewP{})) acc = accumNames q (Set.insert (x, t) acc)
              accumNames (NewP x t (normalForm -> q)) acc = (Set.insert (x, t) acc, q)

    -- (S|A)
    ParP (ParP p q) r -> ParP (normalForm p) (ParP (normalForm q) (normalForm r))

    -- (Sν|)
    -- ParP p (NewP x t q) | x `Set.notMember` freeNames Name' p -> NewP x t (ParP (normalForm p) (normalForm q))

    NilP -> NilP
    ParP p q -> ParP (normalForm p) (normalForm q)
    OutputP x y p -> OutputP x y (normalForm p)
    InputP x y p -> InputP x y (normalForm p)
    ReplicateP x y p -> ReplicateP x y (normalForm p)
    InjectP x f p -> InjectP x f (normalForm p)
    CaseP x ps -> CaseP x (fmap normalForm <$> ps)
    CallP p xs -> CallP p xs
    SourceP x u p -> SourceP x u (normalForm p)

instance Contextual Process Name where
    freeNames d = \case
        NilP             -> mempty
        ParP         p q -> freeNames d p <> freeNames d q
        NewP       x t p -> freeNames d p \\ singleton x
        OutputP    x y p -> freeNames d x <> freeNames d y <> freeNames d p
        InputP     x y p -> freeNames d x <> (freeNames d p \\ singleton y)
        ReplicateP x y p -> freeNames d x <> (freeNames d p \\ singleton y)
        InjectP    x f p -> freeNames d x <> freeNames d p
        CaseP      x  ps -> freeNames d x <> foldMap (freeNames d) (Compose ps)
        CallP      p  xs -> foldMap (freeNames d) xs
        SourceP    x u p -> singleton x <> singleton u <> freeNames d p

    substitute s@(Subst m) = \case
        NilP             -> NilP
        ParP         p q -> ParP (substitute s p) (substitute s q)
        NewP       x t p -> NewP x t (substitute (restrict x s) p)
        OutputP    x y p -> OutputP (substitute s x) (substitute s y) (substitute s p)
        InputP     x y p -> InputP (substitute s x) y (substitute (restrict y s) p)
        ReplicateP x y p -> ReplicateP (substitute s x) y (substitute (restrict y s) p)
        InjectP    x f p -> InjectP (substitute s x) f (substitute s p)
        CaseP      x  ps -> CaseP (substitute s x) (fmap (substitute s) <$> ps)
        CallP      p  xs -> CallP p (substitute s <$> xs)
        SourceP    x u p -> case Map.lookup x m of
            Just (VarN y) -> SourceP y u (substitute (restrict u s) p)
            Just y -> substitute1 u y p
            Nothing -> SourceP x u (substitute (restrict u s) p)
        

instance Dual Type where
    dual = \case
        DualT (DualT a) -> a
        DualT a -> dual a
        ZeroT -> TopT
        TopT -> ZeroT
        OneT -> BottomT
        BottomT -> OneT
        OfCourseT t -> WhyNotT (dual t)
        WhyNotT t -> OfCourseT (dual t)
        TensorT a b -> ParT (dual a) (dual b)
        ParT a b -> TensorT (dual a) (dual b)
        PlusT fs -> WithT (dual <$> fs)
        WithT fs -> PlusT (dual <$> fs)
        PrimT t -> DualT (PrimT t)
        VarT n -> DualT (VarT n)
        IdentT n -> DualT (IdentT n)

instance Unifiable Type Type where
    unify t t' = unifying t t' $ case (t, t') of
        -- Dual
        (DualT (DualT t), t') -> unify t t'
        (DualT t, DualT t') -> unify t t'
        (DualT t, t') -> unify t (dual t')
        (t, DualT t') -> unify (dual t) t'

        -- Bindings
        (VarT n, t) -> pure (Subst (Map.singleton n t))
        (t, VarT n) -> pure (Subst (Map.singleton n t))
        (IdentT n, IdentT n') | n == n' -> pure mempty
        (IdentT n, t) -> flip unify t =<< instantiate =<< lookupTau n
        (t, IdentT n) -> unify t =<< instantiate =<< lookupTau n

        -- Units
        (TopT, TopT) -> pure mempty
        (BottomT, BottomT) -> pure mempty
        (OneT, OneT) -> pure mempty
        (ZeroT, ZeroT) -> pure mempty

        -- Modal operators
        (OfCourseT t, OfCourseT t') -> unify t t'
        (WhyNotT t, WhyNotT t') -> unify t t'

        -- Connectives
        (TensorT a b, TensorT a' b') -> (<>) <$> unify a a' <*> unify b b'
        (ParT a b, ParT a' b') -> (<>) <$> unify a a' <*> unify b b'
        (WithT fs, WithT fs') -> (unify `on` Env) fs fs'
        (PlusT fs, PlusT fs') -> (unify `on` Env) fs fs'

        -- Primitive types
        (PrimT t, PrimT t') | t == t' -> pure mempty

        -- Failure
        (t, t') -> throwError (InferError $ "Unable to unify types" <+> string (show t) <+> "and" <+> string (show t'))

instance Unifiable Context Type where
    unify (Context d t) (Context d' t') = (<>) <$> unify d d' <*> unify t t'
        
unifyCtx :: Context -> Context -> Infer Context
unifyCtx (Context d t) (Context d' t') = Context <$> unifyEnv d d' <*> unifyEnv t t'

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
            `catchError` \ e -> trace (name ++ ": " ++ show e) (throwError e)

unifying :: (Show a, Unifiable a a) => a -> a -> Infer (Subst a) -> Infer (Subst a)
unifying t t' = indentDebug . flip \ tab -> trace (tab <> show t <> " ~ " <> show t')

-- findCuts :: 

instance Inferable Process where
    type TypeOf Process = Type

    infer j = (j & judged %~ normalForm) & \case
        -- (Tcall)
        CallP p ns :⊢ ctx -> rule j "(Tcall)" do
            (ctx <>) <$> (instantiateCall ns =<< lookupGamma p)

        -- (T1)
        NilP :⊢ ctx -> rule j "(T1)" $ ctx <$ complete ctx

        -- (T⅋)
        InputP (VarN x) y p :⊢ ctx -> rule j "(T⅋)" do
            ctx' <- infer (p :⊢ ctx)
            (a, b) <- (,) <$> lookupDeltaBottom y ctx' <*> lookupDelta x ctx'
            pure (ctx' & delta %~ introduce x (ParT a b) . remove y)

        -- (T⊗)
        NewP y ma (OutputP (VarN x) (VarN y') (ParP p q)) :⊢ ctx | y == y' -> rule j "(T⊗)" do
            a <- maybe (freshType "A") pure ma
            ctxp <- infer (p :⊢ mempty)
            a' <- lookupDelta y ctxp
            subst =<< unify a a'
            ctxq <- infer (q :⊢ mempty)
            b <- lookupDeltaOne x ctxq
            pure (ctxp <> ctxq & delta %~ introduce x (TensorT a b))

        -- (Tcut?)
        ContractP u y p q :⊢ ctx -> rule j "(Tcut?)" do
            ctxp <- infer (p :⊢ ctx & delta .~ mempty)
            a' <- lookupDelta y ctxp
            complete (ctxp & delta %~ remove y)
            ctxq <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            s <- unify @Type @Type (dual a) a'
            pure (ctxp <> ctxq)

        -- (Tcopy)
        NewP y a (OutputP (VarN u) (VarN y') p) :⊢ ctx | y == y' -> rule j "(Tcopy)" do
            ctxp <- infer (p :⊢ mempty)
            unifyCtx ctx (ctxp & (delta %~ remove y))

        -- (Tcut)
        NewP x a (ParP p q) :⊢ ctx -> rule j "(Tcut)" do
            ctxp <- infer (p :⊢ ctx)
            ctxq <- infer (q :⊢ ctx)
            a' <- lookupDelta x ctxp
            a <- lookupDelta x ctxq
            s <- unify @Type @Type (dual a) a'
            (unifyCtx `on` delta %~ remove x) ctxp ctxq

        -- (T!)
        ReplicateP (VarN x) y q :⊢ ctx -> rule j "(T!)" do
            ctxq <- infer (q :⊢ mempty)
            a <- lookupDelta y ctxq
            pure $ (ctxq <> ctx & delta %~ introduce x (OfCourseT a) . remove y)

        -- (T⊕)
        InjectP (VarN x) l p :⊢ ctx -> rule j "(T⊕)" do
            ctxp <- infer (p :⊢ mempty)
            a <- lookupDelta x ctxp
            pure (ctx <> ctxp & delta %~ introduce x (PlusT (Map.singleton l a)))

        -- (T&)
        CaseP (VarN x) alts :⊢ ctx -> rule j "(T&)" do
            (fs, ctxs) <- unzip <$> forM alts \ (l, p) -> do
                ctxp <- infer (p :⊢ mempty)
                a <- lookupDelta x ctxp
                ctx' <- unifyCtx ctx (ctxp & delta %~ remove x)
                pure ((l, a), ctx')
            pure (mconcat ctxs & delta %~ introduce x (WithT (Map.fromList fs)))

        -- (T?)
        SourceP x u p :⊢ ctx -> rule j "(T?)" do
            ctxp <- infer (p :⊢ ctx)
            a <- lookupTheta u ctxp
            pure $ (ctxp & theta %~ remove u)
                <> (ctx  & delta %~ introduce x (WhyNotT a))

        (p :⊢ ctx) -> throwError (InferError $ "Couldn't infer process" <+> string (show (j ^. judged)))


