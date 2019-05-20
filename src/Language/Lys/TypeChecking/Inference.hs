{-# LANGUAGE
    LambdaCase
  , TupleSections
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
import qualified Language.Lys.Parser.AST as AST
import Language.Lys.Core
import Language.Lys.Desugar
import Language.Lys.Types
import Language.Lys.Types.Env
import Language.Lys.Types.Context

import Control.Arrow ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Functor.Compose
import Data.These
import Data.Align.Key

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Control.Lens hiding (Context)

import Debug.Trace

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
    MatchP x bs -> MatchP x $ bs <&> \ (Branch l y p) -> Branch l y (normalForm p)
    CallP p xs -> CallP p xs
    SourceP x u p -> SourceP x u (normalForm p)

instance Contextual a c => Contextual (Branch a) c where
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
        MatchP     x  bs -> freeNames d x <> foldMap (freeNames d) bs
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
        MatchP     x  bs -> MatchP (substitute s x) (substitute s <$> bs)
        CallP      p  xs -> CallP p (substitute s <$> xs)
        SourceP    x u p -> case Map.lookup x m of
            Just (VarN y) -> SourceP y u (substitute (restrict u s) p)
            Just y -> substitute1 u y p
            Nothing -> SourceP x u (substitute (restrict u s) p)
            
instance Contextual Process Type where
    freeNames d = \case
        NilP             -> mempty
        ParP         p q -> freeNames d p <> freeNames d q
        NewP       x t p -> foldMap (freeNames d) t <> freeNames d p
        OutputP    x y p -> freeNames d p
        InputP     x y p -> freeNames d p
        ReplicateP x y p -> freeNames d p
        InjectP  x f y p -> freeNames d p
        MatchP     x  bs -> foldMap (freeNames d) bs
        CallP      p  xs -> mempty
        SourceP    x u p -> freeNames d p

    substitute s = \case
        NilP             -> NilP
        ParP         p q -> ParP (substitute s p) (substitute s q)
        NewP       x t p -> NewP x (substitute s <$> t) (substitute s p)
        OutputP    x y p -> OutputP x y (substitute s p)
        InputP     x y p -> InputP x y (substitute s p)
        ReplicateP x y p -> ReplicateP x y (substitute s p)
        InjectP  x f (VarN y) p -> InjectP x f (VarN y) (substitute s p)
        InjectP  x f (LitN y) p -> InjectP x f (LitN y) (substitute s p)
        MatchP     x  bs -> MatchP x (substitute s <$> bs)
        CallP      p  xs -> CallP p xs
        SourceP    x u p -> SourceP x u (substitute s p)

rigidify :: [String] -> Process -> Process
rigidify = substitute . Subst . Map.fromList . map \ tv -> (tv, RigidT tv)

relax :: Type -> Type
relax = \case
    TopT -> TopT
    BottomT -> BottomT
    OneT -> OneT
    ZeroT -> ZeroT
    OfCourseT t -> OfCourseT (relax t)
    WhyNotT t -> WhyNotT (relax t)
    TensorT a b -> TensorT (relax a) (relax b)
    ParT a b -> ParT (relax a) (relax b)
    PlusT fs -> PlusT (relax <$> fs)
    WithT fs -> WithT (relax <$> fs)
    VarT n -> VarT n
    RigidT n -> VarT n
    IdentT n -> IdentT n
    DualT t -> DualT (relax t)
    AppT t ts -> AppT (relax t) (relax <$> ts)
    PrimT t -> PrimT t

(∆) :: Ord k => Map.Map k v -> Map.Map k v -> Map.Map k v
m ∆ m' = Map.withoutKeys m (Map.keysSet m') <> Map.withoutKeys m' (Map.keysSet m)

instance Unifiable Type Type where
    unify = unifying \case
        -- Bindings
        (VarT n, VarT n')
            | n == n'   -> pure mempty
            | otherwise -> pure (Subst (Map.singleton n (VarT n')))
        (VarT n, t) -> pure (Subst (Map.singleton n t))
        (t, VarT n) -> pure (Subst (Map.singleton n t))

        -- Duality
        (DualT (DualT t), t') -> unify t t'
        (DualT t, DualT t') -> unify t t'
        (DualT t, t') -> unify t (dual t')
        (t, DualT t') -> unify (dual t) t'

        -- Rigid type variables
        (RigidT n, RigidT n') | n == n' -> pure mempty

        -- Identifiers
        (IdentT n, IdentT n') | n == n' -> pure mempty
        (IdentT n, t) -> flip unify t =<< instantiate =<< lookupTau n
        (t, IdentT n) -> unify t =<< instantiate =<< lookupTau n

        -- Generics
        (AppT t ts, AppT t' ts') -> (mconcat .) . (:) <$> unify t t' <*> zipWithM unify ts ts'
        (AppT (DualT (IdentT n)) ts, t') -> unify (AppT (IdentT n) (map dual ts)) (DualT t')
        (t, AppT (DualT (IdentT n')) ts') -> unify (DualT t) (AppT (IdentT n') (map dual ts'))
        (AppT (IdentT n) ts, t') -> do
            Scheme tv tx <- lookupTau n
            let s = Subst (Map.fromList (zip tv ts))
            if length ts == length tv then
                unify (substitute s (relax tx)) t'
            else
                throwError1 (TypeError $ "Type constructor" <+> string n <+> "expected" <+> int (length tv) <+> "arguments, got" <+> int (length ts))
        (t, AppT (IdentT n') ts') -> do
            Scheme tv' tx' <- lookupTau n'
            let s = Subst (Map.fromList (zip tv' ts'))
            if length ts' == length tv' then
                unify t (substitute s (relax tx'))
            else
                throwError1 (TypeError $ "Type constructor" <+> string n' <+> "expected" <+> int (length tv') <+> "arguments, got" <+> int (length ts'))

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
        (PlusT fs, PlusT fs') -> unifyFields fs fs'
        (WithT fs, WithT fs') -> unifyFields fs fs'

        -- Primitive types
        (PrimT t, PrimT t') | t == t' -> pure mempty

        -- Failure
        (t, t') -> throwError1 (TypeError $ "Unable to unify types" <+> pretty t <+> "and" <+> pretty t')

unifyFields :: Fields -> Fields -> Infer (Subst Type)
unifyFields fs fs' = fold <$> sequence (alignWithKey f fs fs')
    where f k = these (const err) (const err) unify
          err = throwError1 (TypeError $ "Couldn't unify fields" <+> pretty fs <+> "and" <+> pretty fs')

unifiedFields :: Fields -> Fields -> Infer (Fields, Subst Type)
unifiedFields fs fs' = (_2 %~ fold) . unzipF <$> sequence (alignWithKey f fs fs')
    where f _ = these (pure . (,mempty)) (pure . (,mempty)) unified

instance Unified Type Type where
    unified = curry \case
        -- Bindings
        (VarT n, VarT n')
            | n == n'   -> pure (VarT n, mempty)
            | otherwise -> pure (VarT n', Subst (Map.singleton n (VarT n')))
        (VarT n, t) -> pure (t, Subst (Map.singleton n t))
        (t, VarT n) -> pure (t, Subst (Map.singleton n t))

        -- Rigid type variables
        (RigidT n, RigidT n') | n == n' -> pure (RigidT n, mempty)
        (RigidT n, t') -> throwError1 (TypeError $ "Unable to unify types" <+> pretty (RigidT n) <+> "and" <+> pretty t' <+> ", type variable" <+> text n <+> "is rigid")
        (t, RigidT n') -> throwError1 (TypeError $ "Unable to unify types" <+> pretty (RigidT n') <+> "and" <+> pretty t <+> ", type variable" <+> text n' <+> "is rigid")

        -- Identifiers
        (IdentT n, IdentT n') | n == n' -> pure (IdentT n, mempty)
        (IdentT n, t) -> do
            (_, s) <- flip unified t =<< instantiate =<< lookupTau n
            pure (IdentT n, s)
        (t, IdentT n) -> do
            (_, s) <- unified t =<< instantiate =<< lookupTau n
            pure (IdentT n, s)

        -- Duality
        (DualT (DualT t), t') -> unified t t'
        (DualT t, DualT t') -> (_1 %~ dual) <$> unified t t'
        (DualT t, t') -> (_1 %~ dual) <$> unified t (dual t')
        (t, DualT t') -> (_1 %~ dual) <$> unified (dual t) t'

        -- Generics
        (AppT t ts, AppT t' ts') -> do
            (t'', s1) <- unified t t'
            (ts'', s2) <- (_2 %~ mconcat) . unzip <$> zipWithM unified ts ts'
            pure (AppT t'' ts'', s2 <> s1)
        (AppT (IdentT n) ts, t') -> do
            Scheme tv tx <- lookupTau n
            if length ts == length tv then
                unified (substitute (Subst (Map.fromList (zip tv ts))) tx) t'
            else
                throwError1 (TypeError $ "Type constructor" <+> string n <+> "expected" <+> int (length tv) <+> "arguments, got" <+> int (length ts))
        (t, AppT (IdentT n') ts') -> do
            Scheme tv' tx' <- lookupTau n'
            -- until I have real kind checking
            if length ts' == length tv' then
                unified t (substitute (Subst (Map.fromList (zip tv' ts'))) tx')
            else
                throwError1 (TypeError $ "Type constructor" <+> string n' <+> "expected" <+> int (length tv') <+> "arguments, got" <+> int (length ts'))

        -- Disjunctions
        (PlusT fs, PlusT fs') -> (_1 %~ PlusT) <$> unifiedFields fs fs'
        (WithT fs, WithT fs') -> (_1 %~ WithT) <$> unifiedFields fs fs'
        (t, t') -> do
            s <- unify @Type @Type t t'
            pure (substitute s t', s)

instance Unifiable Context Type where
    unify (Context d t) (Context d' t') = (<>) <$> unify d d' <*> unify t t'
        
instance Unified Context Type where
    unified (Context d t) (Context d' t') = do
        (d'', s1) <- unified d d'
        (t'', s2) <- unified t t'
        pure (Context d'' t'', s2 <> s1)

instance Unifiable (Scheme Type Type) Type where
    unify s@(Scheme vs t) s'@(Scheme vs' t') = do
        subst <- unify t t'
        onlyNameSubst subst
        pure subst
        where err = throwError1 (TypeError $ "Schemes" <+> pretty s <+> "and" <+> pretty s' <+> "do not unify")
              onlyNameSubst (Subst s) = Subst . Map.fromList <$> forM (Map.assocs s) \ (n, m) ->
                flip maybe (pure . (n,)) err (m ^? var)

instance Unified (Scheme Type Type) Type where
    unified s@(Scheme vs t) s'@(Scheme vs' t') = do
        (t'', subst) <- unified t t'
        subst'' <- onlyNameSubst subst
        let s'' = Scheme (substitute subst'' <$> vs) t''
        pure (s'', subst)
        where err = throwError1 (TypeError $ "Schemes" <+> pretty s <+> "and" <+> pretty s' <+> "do not unify")
              onlyNameSubst (Subst s) = Subst . Map.fromList <$> forM (Map.assocs s) \ (n, m) ->
                flip maybe (pure . (n,)) err (m ^? var)

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
            ctx' <- instantiateCall ns =<< lookupGamma p
            unified ctx ctx'

        -- (T1)
        NilP :⊢ ctx -> rule j "(T1)" $ pure (ctx, mempty)

        -- (T⅋)
        InputP (VarN x) y p :⊢ ctx -> rule j "(T⅋)" do
            ab <- lookupDelta x ctx
            a <- freshType "A"
            b <- freshType "A"
            (ab', s1) <- unified @Type @Type ab (ParT a b)
            (ctx', s2) <- infer (p :⊢ substitute s1 (ctx & delta %~ introduce x b))
            (a', b') <- (,) <$> (lookupDelta y ctx' <|> freshType "A") <*> lookupDelta x ctx'
            (a'', s3) <- (unified @Type @Type `on` substitute (s2 <> s1)) a a'
            (b'', s4) <- (unified @Type @Type `on` substitute (s3 <> s2 <> s1)) b b'
            -- a <- unified ab (ParT a'' b'')
            pure (ctx' & delta %~ introduce x (ParT a'' b'') . remove y, s4 <> s3 <> s2 <> s1)

        -- (T⊗)
        NewP y ma (OutputP (VarN x) (VarN y') (ParP p q)) :⊢ ctx | y == y' -> rule j "(T⊗)" do
            a <- maybe (freshType "A") pure ma
            (ctxp, s1) <- infer (p :⊢ mempty)
            a' <- lookupDelta y ctxp
            s <- unify @Type @Type a a'
            (ctxq, s2) <- infer (q :⊢ mempty)
            b <- lookupDelta x ctxq {-} <|> freshType "B" --}
            (ctx', s3) <- unified ctx ((ctxp & delta %~ remove y) <> (ctxq & delta %~ remove x))
            let s = s3 <> s2 <> s1
            pure (substitute s (ctx' & delta %~ introduce x (TensorT a b)), s)

        -- (Tcut?)
        ContractP u y p q :⊢ ctx -> rule j "(Tcut?)" do
            (ctxp, s1) <- infer (p :⊢ ctx & delta .~ mempty)
            a' <- lookupDelta y ctxp
            complete (ctxp & delta %~ remove y)
            (ctxq, s2) <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            s3 <- unify @Type @Type (dual a) a'
            pure (substitute s3 (ctxp <> ctxq), s3 <> s2 <> s1)

        -- (Tcopy) + (T⊗)
        NewP y ma (OutputP (VarN u) (VarN y') p) :⊢ ctx | y == y' -> do
            a <- maybe (freshType "A") pure ma
            (<|>)
                (rule j "(Tcopy)" $ do (ctxp, s1) <- infer (p :⊢ ctx)
                                       a' <- lookupTheta u ctxp
                                       s2 <- unify @Type @Type a a'
                                       (ctx', s3) <- unified ctx (ctxp & delta %~ remove y)
                                       pure (ctx' & theta %~ introduce u a, s3 <> s2 <> s1))
                (rule j "(T⊗)" $ do (ctxp, s1) <- infer (p :⊢ ctx & delta %~ introduce u a)
                                    a' <- lookupDeltaOne u ctxp
                                    (a'', s2) <- unified @Type @Type a a'
                                    (ctx', s3) <- unified ctx (ctxp & delta %~ remove y)
                                    pure (ctx' & delta %~ introduce u a'', s3 <> s2 <> s1))

        -- (Tcut)
        NewP x ma (ParP p q) :⊢ ctx -> rule j "(Tcut)" do
            a <- maybe (freshType "A") pure ma
            (ctxp, s1) <- infer (p :⊢ ctx)
            (ctxq, s2) <- infer (q :⊢ ctx)
            a' <- lookupDelta x ctxp
            ad <- lookupDelta x ctxq
            (a'', s3) <- unified @Type @Type a a'
            s4 <- unify @Type @Type (dual a'') ad
            let s = mconcat [s4,s3,s2,s1]
            (ctx', s') <- (unified `on` delta %~ remove x) ctxp ctxq
            pure (ctx', s' <> s)

        -- (Tcomp)
        -- ParP p q :⊢ ctx -> rule j "(Tcomp)" do
        --     (ctxp, s1) <- infer (p :⊢ ctx)
        --     (ctxq, s2) <- infer (q :⊢ ctx)
        --     (ctx', s3) <- unified ctx (substitute (s2 <> s1) (ctxp <> ctxq))
        --     pure (ctx', s3 <> s2 <> s1)

        -- (T!)
        ReplicateP (VarN x) y q :⊢ ctx -> rule j "(T!)" do
            (ctxq, s1) <- infer (q :⊢ ctx)
            a <- lookupDelta y ctxq
            (ctx', s2) <- (unified `on` substitute s1) ctx (ctxq & delta %~ introduce x (OfCourseT a) . remove y)
            pure (ctx', s2 <> s1)

        -- (T⊕)
        InjectP (VarN x) l ny p :⊢ ctx -> rule j "(T⊕)" do
            ab <- lookupDelta x ctx <|> freshType "A"
            (ctxp, s1) <- infer (p :⊢ ctx)
            a <- case ny of
                LitN l -> pure (inferLiteral l)
                VarN y -> lookupDelta y ctxp
            (ab', s2) <- (unified `on` substitute s1) (PlusT (Map.singleton l a)) ab
            (ctx', s3) <- (unified `on` substitute (s2 <> s1)) (ctx & delta %~ remove x) (ctxp & delta %~ introduce x ab')
            pure (ctx', s3 <> s2 <> s1)

        -- (T&)
        MatchP (VarN x) alts :⊢ ctx -> rule j "(T&)" do
            t <- lookupDelta x ctx
            (fs, ctxSb) <- unzip <$> forM alts \ (Branch l pat p) -> case pat of
                WildcardPat -> do
                    (ctxp, s1) <- infer (p :⊢ ctx)
                    (ctx', s2) <- (unified `on` substitute s1) ctx (ctxp & delta %~ remove x)
                    pure ((l, BottomT), (ctx', s2 <> s1))
                VarPat y -> do
                    a <- freshType "A"
                    (_, s1) <- unified t (WithT (Map.singleton y a))
                    (ctxp, s2) <- infer (p :⊢ ctx & delta %~ introduce y (substitute s1 a))
                    (ctx', s3) <- (unified @Context @Type `on` substitute (s2 <> s1)) ctx (ctxp & delta %~ remove y . remove x)
                    pure ((l, dual (substitute s1 a)), (ctx', s2 <> s1))
            let go [] (accC, accS) = pure (accC, accS)
                go ((c,s):css) (accC, accS) = do
                    (accC', s') <- unified c accC
                    go css (accC', s' <> s <> accS)
            t <- lookupDelta x ctx
            let t' = WithT (Map.fromList fs)
            (t'', s) <- unified t t'
            (ctx'', s') <- (_1.delta %~ introduce x t'') <$> go ctxSb (ctx, s)
            pure (ctx'', s')
            -- unified' Type' ctx' (ctx & delta %~ introduce x (WithT (Map.fromList fs)))

        -- (T?)
        SourceP x u p :⊢ ctx -> rule j "(T?)" do
            a <- lookupDelta x ctx
            a' <- flip (unified' Type') a . WhyNotT =<< freshType "A"
            (ctxp, s1) <- infer (p :⊢ ctx & (theta %~ introduce u a') . (delta %~ remove x))
            a' <- lookupTheta u ctxp -- <|> pure OneT
            (a'', s2) <- unified a a'
            (ctx', s3) <- (unified `on` substitute (s2 <> s1) . (delta %~ remove u)) ctx ctxp
            pure (ctx' & (delta %~ introduce x a'')
                       . (theta %~ remove u), s3 <> s2 <> s1)

        (p :⊢ ctx) -> throwError1 (TypeError $ "Couldn't infer process" <+> string (show (j ^. judged)))

instance Checkable Process Context where
    check = checkInfer . fmap fst . infer . (:⊢ mempty)

instance Inferable AST.Process where
    type TypeOf AST.Process = Type
    infer = infer . (judged %~ desugar)

instance Checkable AST.Process Context where
    check = check . desugar
