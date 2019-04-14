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

module Language.Lys.TypeChecking.Inference where

import Language.Lys.TypeChecking.Types
import Language.Lys.TypeChecking.Debug
import Language.Lys.Types

import Control.Arrow ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Functor.Compose

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap

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
    InjectP x f y p -> InjectP x f y (normalForm p)
    CaseP x bs -> CaseP x $ bs <&> \ (Branch l y p) -> Branch l y (normalForm p)
    CallP p xs -> CallP p xs
    SourceP x u p -> SourceP x u (normalForm p)

instance Contextual Branch Name where
    freeNames d (Branch l py p) = case py of
        WildcardPat -> freeNames d p
        VarPat y    -> freeNames d p \\ singleton y

    substitute s (Branch l (VarPat y) p) = Branch l (VarPat y) (substitute (restrict y s) p)
    substitute s (Branch l WildcardPat p) = Branch l WildcardPat (substitute s p)

instance Contextual Process Name where
    freeNames d = \case
        NilP             -> mempty
        ParP         p q -> freeNames d p <> freeNames d q
        NewP       x t p -> freeNames d p \\ singleton x
        OutputP    x y p -> freeNames d x <> freeNames d y <> freeNames d p
        InputP     x y p -> freeNames d x <> (freeNames d p \\ singleton y)
        ReplicateP x y p -> freeNames d x <> (freeNames d p \\ singleton y)
        InjectP  x f y p -> freeNames d x <> freeNames d y <> freeNames d p
        CaseP      x  bs -> freeNames d x <> foldMap (freeNames d) bs
        CallP      p  xs -> foldMap (freeNames d) xs
        SourceP    x u p -> singleton x <> singleton u <> freeNames d p

    substitute s@(Subst m) = \case
        NilP             -> NilP
        ParP         p q -> ParP (substitute s p) (substitute s q)
        NewP       x t p -> NewP x t (substitute (restrict x s) p)
        OutputP    x y p -> OutputP (substitute s x) (substitute s y) (substitute s p)
        InputP     x y p -> InputP (substitute s x) y (substitute (restrict y s) p)
        ReplicateP x y p -> ReplicateP (substitute s x) y (substitute (restrict y s) p)
        InjectP  x f (VarN y) p -> InjectP (substitute s x) f (VarN y) (substitute (restrict y s) p)
        InjectP  x f (LitN y) p -> InjectP (substitute s x) f (LitN y) (substitute s p)
        CaseP      x  bs -> CaseP (substitute s x) (substitute s <$> bs)
        CallP      p  xs -> CallP p (substitute s <$> xs)
        SourceP    x u p -> case Map.lookup x m of
            Just (VarN y) -> SourceP y u (substitute (restrict u s) p)
            Just y -> substitute1 u y p
            Nothing -> SourceP x u (substitute (restrict u s) p)
            

(∆) :: Ord k => Map.Map k v -> Map.Map k v -> Map.Map k v
m ∆ m' = Map.withoutKeys m (Map.keysSet m') <> Map.withoutKeys m' (Map.keysSet m)

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

        -- Identifiers
        (IdentT n, IdentT n') | n == n' -> pure mempty
        (IdentT n, t) -> flip unify t =<< instantiate =<< lookupTau n
        (t, IdentT n) -> unify t =<< instantiate =<< lookupTau n

        -- Generics
        (AppT t ts, AppT t' ts') -> (mconcat .) . (:) <$> unify t t' <*> zipWithM unify ts ts'
        (AppT (IdentT n) ts, t') -> do
            Scheme tv tx <- lookupTau n
            if length ts == length tv then
                unify (substitute (Subst (Map.fromList (zip tv ts))) tx) t'
            else
                throwError (InferError $ "Type constructor" <+> string n <+> "expected" <+> int (length tv) <+> "arguments, got" <+> int (length ts))
        (t, AppT (IdentT n') ts') -> do
            Scheme tv' tx' <- lookupTau n'
            if length ts' == length tv' then
                unify t (substitute (Subst (Map.fromList (zip tv' ts'))) tx')
            else
                throwError (InferError $ "Type constructor" <+> string n' <+> "expected" <+> int (length tv') <+> "arguments, got" <+> int (length ts'))

        (AppT (DualT (IdentT n)) ts, t') -> unify (AppT (IdentT n) ts) (DualT t')
        (t, AppT (DualT (IdentT n')) ts') -> unify (DualT t) (AppT (IdentT n') ts')

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
        (WithT fs, WithT fs')
            | Map.null (fs ∆ fs') -> (unify `on` Env) fs fs'
        (PlusT fs, PlusT fs')
            | Map.null (fs ∆ fs') -> (unify `on` Env) fs fs'

        -- Primitive types
        (PrimT t, PrimT t') | t == t' -> pure mempty

        -- Failure
        (t, t') -> throwError (InferError $ "Unable to unify types" <+> string (show t) <+> "and" <+> string (show t'))

instance Unified Type where
    unified t@(PlusT fs) (PlusT fs') = do
            Env fs'' <- (unified `on` common) fs fs'
            pure (PlusT (fs'' <> diff))
        where common = Env . flip Map.withoutKeys (Map.keysSet diff)
              diff = fs ∆ fs'
    unified (WithT fs) (WithT fs') = do
            Env fs'' <- (unified `on` common) fs fs'
            pure (WithT (fs'' <> diff))
        where common = Env . flip Map.withoutKeys (Map.keysSet diff)
              diff = fs ∆ fs'
    unified t t' = do
        s <- unify @Type @Type t t'
        pure (substitute s t)

instance Unifiable Context Type where
    unify (Context d t) (Context d' t') = (<>) <$> unify d d' <*> unify t t'
        
instance Unified Context where
    unified (Context d t) (Context d' t') = Context <$> unified d d' <*> unified t t'

data Action
    = OutputA Name Name
    | InputA Name String
    | BoundOutputA String Name
    | InjectA Name String
    | SelectA Name String
    | InternalA
    deriving (Eq, Ord, Show)

(⊥∝) :: Action -> Action -> Bool
OutputA x (VarN y) ⊥∝ InputA x' y' = x == x' && y == y'
InputA x y ⊥∝ OutputA x' (VarN y') = x == x' && y == y'
BoundOutputA x (VarN y) ⊥∝ InputA (VarN x') y' = x == x' && y == y'
InputA (VarN x) y ⊥∝ BoundOutputA x' (VarN y') = x == x' && y == y'
InjectA x l ⊥∝ SelectA x' l' = x == x' && l == l'
SelectA x l ⊥∝ InjectA x' l' = x == x' && l == l'
-- InternalA ⊥∝ InternalA = True
_ ⊥∝ _ = False

-- processAction :: Judgement Process -> Infer Action
-- processAction = \case
--     NilP :⊢ ctx -> throwError (InferError $ "Internal error, nil has no action")
--     ParP p q :⊢ ctx -> do
--         a <- processAction

type CommNet = MMap.MultiMap Action Process

buildCommNet :: Process -> Infer CommNet
buildCommNet p = undefined
    where (cutPoints, cutProc) = foldRestrictions p
          threads = foldPars cutProc

foldPars :: Process -> [Process]
foldPars = \case
    ParP NilP p -> foldPars p
    ParP p NilP -> foldPars p
    ParP p (ParP q r) -> foldPars p <> foldPars q <> foldPars r
    ParP (ParP p q) r -> foldPars p <> foldPars q <> foldPars r
    p -> [p]

foldRestrictions :: Process -> (MMap.MultiMap String (Maybe Type), Process)
foldRestrictions = go mempty
    where go acc (NewP x mt p) = go (MMap.insert x mt acc) p
          go acc p = (acc, p)

inferLiteral :: Literal -> Type
inferLiteral = \case
    IntL{} -> PrimT IntT
    OneL   -> OneT

instance Inferable Process where
    type TypeOf Process = Type

    infer j = (j & judged %~ normalForm) & \case
        -- (Tcall)
        CallP p ns :⊢ ctx -> rule j "(Tcall)" do
            (ctx <>) <$> (instantiateCall ns =<< lookupGamma p)

        -- (T1)
        NilP :⊢ ctx -> rule j "(T1)" $ pure ctx

        -- (T⅋)
        InputP (VarN x) y p :⊢ ctx -> rule j "(T⅋)" do
            ctx' <- infer (p :⊢ ctx)
            (a, b) <- (,) <$> (lookupDelta y ctx' <|> freshType "A") <*> lookupDelta x ctx'
            pure (ctx' & delta %~ introduce x (ParT a b) . remove y)

        -- (T⊗)
        NewP y ma (OutputP (VarN x) (VarN y') (ParP p q)) :⊢ ctx | y == y' -> rule j "(T⊗)" do
            a <- maybe (freshType "A") pure ma
            ctxp <- infer (p :⊢ mempty)
            a' <- lookupDelta y ctxp
            s <- unify @Type @Type a a'
            ctxq <- infer (q :⊢ mempty)
            b <- lookupDelta x ctxq {-} <|> freshType "B" --}
            ctx' <- unified ctx ((ctxp & delta %~ remove y) <> (ctxq & delta %~ remove x))
            pure (substitute s (ctx' & delta %~ introduce x (TensorT a b)))

        -- (Tcut?)
        ContractP u y p q :⊢ ctx -> rule j "(Tcut?)" do
            ctxp <- infer (p :⊢ ctx & delta .~ mempty)
            a' <- lookupDelta y ctxp
            complete (ctxp & delta %~ remove y)
            ctxq <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            s <- unify @Type @Type (dual a) a'
            pure (substitute s (ctxp <> ctxq))

        -- (Tcopy) + (T⊗)
        NewP y ma (OutputP (VarN u) (VarN y') p) :⊢ ctx | y == y' -> rule j "(Tcopy)" do
            a <- maybe (freshType "A") pure ma
            ctxp <- infer (p :⊢ mempty)
            (<|>)
                (rule j "(Tcopy)" $ do a' <- lookupTheta u ctxp
                                       s <- unify @Type @Type a a'
                                       ctx' <- unified ctx (ctxp & delta %~ remove y)
                                       pure (substitute s (ctx' & theta %~ introduce u a)))
                (rule j "(T⊗)" $ do a' <- lookupDeltaOne u ctxp
                                    s <- unify @Type @Type a a'
                                    ctx' <- unified ctx (ctxp & delta %~ remove y)
                                    pure (substitute s (ctx' & delta %~ introduce u a)))

        -- (Tcut)
        NewP x ma (ParP p q) :⊢ ctx -> rule j "(Tcut)" do
            a <- maybe (freshType "A") pure ma
            ctxp <- infer (p :⊢ ctx)
            ctxq <- infer (q :⊢ ctx)
            a' <- lookupDelta x ctxp
            ad <- lookupDelta x ctxq
            s1 <- unify @Type @Type a a'
            s2 <- unify @Type @Type (dual a) ad
            s3 <- unify @Type @Type (dual a') ad
            substitute (s3 <> s2 <> s1) <$> (unified `on` delta %~ remove x) ctxp ctxq

        -- (T!)
        ReplicateP (VarN x) y q :⊢ ctx -> rule j "(T!)" do
            ctxq <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            pure $ (ctxq <> ctx & delta %~ introduce x (OfCourseT a) . remove y)

        -- (T⊕)
        InjectP (VarN x) l ny p :⊢ ctx -> rule j "(T⊕)" do
            a <- lookupDelta x ctx
            ctxp <- infer (p :⊢ ctx)
            a' <- case ny of
                LitN l -> pure (inferLiteral l)
                VarN y -> lookupDelta y ctxp
            s <- unify @Type @Type a a'
            substitute s <$> unified ctx (ctxp & delta %~ introduce x (PlusT (Map.singleton l a)))

        -- (T&)
        CaseP (VarN x) alts :⊢ ctx -> rule j "(T&)" do
            (fs, ctxs) <- unzip <$> forM alts \ (Branch l pat p) -> case pat of
                WildcardPat -> do
                    ctxp <- infer (p :⊢ ctx)
                    ctx' <- unified ctx (ctxp & delta %~ remove x)
                    pure ((l, BottomT), ctx')
                VarPat y -> do
                    a <- freshType "A"
                    ctxp <- infer (p :⊢ ctx & delta %~ introduce y a)
                    --a <- lookupDelta x ctxp
                    b <- lookupDelta y ctxp
                    ctx' <- unified ctx (ctxp & delta %~ remove y . remove x)
                    pure ((l, b), ctx')
            ctx' <- case ctxs of
                c:cs -> foldM unified c cs
                []   -> pure mempty
            unified ctx' (mempty & delta %~ introduce x (WithT (Map.fromList fs)))

        -- (T?)
        SourceP x u p :⊢ ctx -> rule j "(T?)" do
            ctxp <- infer (p :⊢ ctx)
            a <- lookupTheta u ctxp -- <|> pure OneT
            pure $ (ctxp & theta %~ remove u)
                <> (ctx  & delta %~ introduce x (WhyNotT a))

        (p :⊢ ctx) -> throwError (InferError $ "Couldn't infer process" <+> string (show (j ^. judged)))


