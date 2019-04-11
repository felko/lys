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
    VarP n -> VarP n
    AppP p x -> AppP p x
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
        VarP           n -> mempty
        AppP         p x -> freeNames d x
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
        VarP           n -> VarP n
        AppP         p x -> AppP p (substitute s x)
        SourceP    x u p -> case Map.lookup x m of
            Just (VarN y) -> SourceP y u (substitute (restrict u s) p)
            Just y -> substitute1 u y p
            Nothing -> SourceP x u (substitute (restrict u s) p)

instance Contextual Process Process where
    freeNames d = \case
        NilP             -> mempty
        ParP         p q -> freeNames d p <> freeNames d q
        NewP       x t p -> freeNames d p
        OutputP    x y p -> freeNames d p
        InputP     x y p -> freeNames d p
        ReplicateP x y p -> freeNames d p
        InjectP    x f p -> freeNames d p
        CaseP      x  ps -> foldMap (freeNames d) (Compose ps)
        VarP           n -> singleton n
        AppP         p x -> singleton p
        SourceP    x u p -> freeNames d p

    substitute s@(Subst m) = \case
        NilP             -> NilP
        ParP         p q -> ParP (substitute s p) (substitute s q)
        NewP       x t p -> NewP x t (substitute s p)
        OutputP    x y p -> OutputP x y (substitute s p)
        InputP     x y p -> InputP x y (substitute s p)
        ReplicateP x y p -> ReplicateP x y (substitute s p)
        InjectP    x f p -> InjectP x f (substitute s p)
        CaseP      x  ps -> CaseP x (fmap (substitute s) <$> ps)
        AppP         p x -> AppP p x
        VarP           n -> case Map.lookup n m of
            Just p -> p
            _ -> VarP n
        SourceP    x u p -> SourceP x u (substitute s p)
        

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
        PlusT a b -> WithT (dual a) (dual b)
        WithT a b -> PlusT (dual a) (dual b)
        PrimT t -> DualT (PrimT t)
        VarT n -> DualT (VarT n)

instance Unifiable Type Type where
    unify t t' = case (t, t') of
        -- Dual
        (DualT (DualT t), t') -> unify t t'
        (DualT t, DualT t') -> unify t t'
        (DualT t, t') -> unify t (dual t')
        (t, DualT t') -> unify (dual t) t'

        -- Bindings
        (VarT n, t) -> pure (Subst (Map.singleton n t))
        (t, VarT n) -> pure (Subst (Map.singleton n t))

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
        (WithT a b, WithT a' b') -> (<>) <$> unify a a' <*> unify b b'
        (PlusT a b, PlusT a' b') -> (<>) <$> unify a a' <*> unify b b'

        -- Primitive types
        (PrimT t, PrimT t') | t == t' -> pure mempty

        -- Failure
        (t, t') -> throwError (InferError $ "Unable to unify types" <+> string (show t) <+> "and" <+> string (show t'))

instance Unifiable Context Type where
    unify (Context d t) (Context d' t') = (<>) <$> unify d d' <*> unify t t'
        
unifyCtx :: Context -> Context -> Infer Context
unifyCtx ctx ctx' = do
    s <- unify @Context @Type ctx ctx'
    pure (substitute @Context @Type s ctx)

rule :: (Show a, Show b) => Judgement b -> String -> Infer a -> Infer a
rule (p :⊢ ctx) name i = trace (name ++ " " ++ show p ++ "\n\t" ++ show ctx) $
    (i >>= \ x -> trace ('/':name ++ " " ++ show x) $ pure x)
        `catchError` \ e -> trace (name ++ ": " ++ show e) (throwError e)

concatContexts :: Context -> Context -> Infer Context
concatContexts ctx ctx' = undefined -- concat deltas, unify thetas

-- findCuts :: 

instance Inferable Process where
    type TypeOf Process = Type

    infer j = (j & judged %~ normalForm) & \case
        -- (Tvar)
        VarP n :⊢ ctx -> rule j "(Tvar)" $ do
            ctx' <- instantiate =<< lookupGamma n
            pure (ctx <> ctx')

        -- (Tapp)
        AppP p n :⊢ ctx -> rule j "(Tapp)" $ lookupGamma p >>= \case
            Scheme _ [] _ -> throwError (InferError $ "Cannot apply name" <+> string (show n) <+> "on non-polymorphic process" <+> string p)
            Scheme tv (m:nv) ctx' -> (ctx <>) <$> instantiate (Scheme tv nv (substitute1 m n ctx'))

        -- (T1)
        NilP :⊢ ctx -> rule j "(T1)" $ pure mempty

        -- (T⅋)
        InputP (VarN x) y p :⊢ ctx -> rule j "(T⅋)" $ do
            ctx' <- infer (p :⊢ mempty)
            (a, b) <- (,) <$> lookupDeltaBottom y ctx' <*> lookupDelta x ctx'
            pure (ctx' & delta %~ introduce x (ParT a b) . remove y)

        -- (T⊗)
        NewP y a (OutputP (VarN x) (VarN y') (ParP p q)) :⊢ ctx | y == y' -> rule j "(T⊗)" $ do
            ctxp <- infer (p :⊢ mempty)
            a' <- lookupDelta y ctxp
            subst =<< unify a a'
            ctxq <- infer (q :⊢ mempty)
            b <- lookupDeltaOne x ctxq
            pure (ctxp <> ctxq & delta %~ introduce x (TensorT a b))

        -- (Tcut?)
        ContractP u y p q :⊢ ctx -> rule j "(Tcut?)" $ do
            ctxp <- infer (p :⊢ ctx & delta .~ mempty)
            a' <- lookupDelta y ctxp
            complete (ctxp & delta %~ remove y)
            ctxq <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            s <- unify @Type @Type (dual a) a'
            pure (ctxp <> ctxq)

        -- (Tcopy)
        NewP y a (OutputP (VarN u) (VarN y') p) :⊢ ctx | y == y' -> rule j "(Tcopy)" $ do
            ctxp <- infer (p :⊢ mempty)
            unifyCtx ctx (ctxp & (delta %~ remove y))

        -- (Tcut)
        NewP x a (ParP p q) :⊢ ctx -> rule j "(Tcut)" $ do
            ctxp <- infer (p :⊢ mempty)
            ctxq <- infer (q :⊢ mempty)
            s <- (unify @Context @Type `on` delta %~ remove x) ctxp ctxq
            a' <- lookupDelta x ctxp
            a <- lookupDelta x ctxq
            s <- unify @Type @Type (dual a) a'
            unifyCtx (ctxp & delta %~ remove x)
                     (ctxq & delta %~ remove x)

        -- (T!)
        ReplicateP (VarN x) y q :⊢ ctx -> rule j "(T!)" $ do
            ctxq <- infer (q :⊢ mempty)
            a <- lookupDelta y ctxq
            pure $ (ctxq <> ctx & delta %~ introduce x (OfCourseT a) . remove y)

        -- (T⊕1)
        

        -- (T⊕2)

        -- (T&)

        -- (T?)
        SourceP x u p :⊢ ctx -> rule j "(T?)" $ do
            ctxp <- infer (p :⊢ mempty)
            a <- lookupTheta u ctxp
            pure $ (ctxp & theta %~ remove u)
                <> (ctx  & delta %~ introduce x (WhyNotT a))

        (p :⊢ ctx) -> throwError (InferError $ "Couldn't infer process" <+> string (show (j ^. judged)))


