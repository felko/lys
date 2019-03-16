{-# LANGUAGE
    LambdaCase
  , TupleSections
  , GADTs
  , KindSignatures
  , FlexibleInstances
  , FlexibleContexts
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , Rank2Types
  , TypeApplications
  , TypeFamilies
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  #-}

module Language.Lys.Inference where

import Language.Lys.Pretty
import Language.Lys.Types

import Data.Traversable
import Data.Functor.Compose (Compose(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Foldable (fold)
import Data.Function (on)
import Data.Proxy
import Data.Maybe
import Data.List (permutations)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), empty)

import Debug.Trace

data KindProxy :: * -> * where
    Type'       :: KindProxy Type
    Session'    :: KindProxy Session
    Name'       :: KindProxy Name
    RecordType' :: KindProxy RecordType
    Record'     :: KindProxy Record
    Of          :: forall k. KindProxy k

data FreeVars = FreeVars
    { _freeSessionVars :: Set.Set String
    , _freeTypeVars    :: Set.Set String
    , _freeNames       :: Set.Set String }
    deriving (Show)
makeLenses ''FreeVars

(\\) :: FreeVars -> FreeVars -> FreeVars
FreeVars sv1 tv1 fn1 \\ FreeVars sv2 tv2 fn2 = FreeVars (Set.difference sv1 sv2) (Set.difference tv1 tv2) (Set.difference fn1 fn2)

data Scheme k = Scheme
    { _boundVars  :: FreeVars
    , _schemeKind :: k }
    deriving (Show, Functor, Foldable)
makeLenses ''Scheme

data Context = Context
    { _typeNames    :: Map.Map String (Scheme Type)
    , _nameBindings :: Map.Map String (Scheme Name)
    , _typeEnv      :: Map.Map String (Scheme Type)
    , _sessionEnv   :: Map.Map String (Scheme Session) }
    deriving (Show)
makeLenses ''Context

data InferState = InferState
    { _typeVarSupply    :: Int
    , _sessionVarSupply :: Int
    , _nameSupply       :: Int }
    deriving (Show)
makeLenses ''InferState

data InferError = InferError Doc
    deriving Show

instance Semigroup InferError where
    (<>) = flip const

instance Monoid InferError where
    mempty = InferError "Unknown error"
    mappend = (<>)

newtype Infer a = Infer
    { unInfer :: ReaderT Context (StateT InferState (Except InferError)) a }
    deriving
        ( Functor, Applicative, Alternative, Monad
        , MonadReader Context
        , MonadState InferState
        , MonadError InferError )

class Kind k where
    freeVars   :: k -> FreeVars
    substitute :: Subst -> k -> k

instance (Foldable f, Functor f, Kind k) => Kind (f k) where
    freeVars = foldMap freeVars
    substitute = fmap . substitute
    
class Kind k => Inferable k where
    type Expr k :: *
    freeVarsSet  :: proxy k -> Lens' FreeVars (Set.Set String)
    varSupply    :: proxy k -> Lens' InferState Int
    varConstr    :: String -> k
    unify        :: k -> k -> Infer Subst
    infer        :: Expr k -> Infer (Subst, k)

unifyWithSubst :: forall k. Inferable k => Subst -> k -> k -> Infer Subst
unifyWithSubst s = unify `on` substitute s

class Inferable k => Substructural k where
    substruct    :: k -> k -> Infer Subst

substructWithSubst :: Substructural k => Subst -> k -> k -> Infer Subst
substructWithSubst s = substruct `on` substitute s

class Inferable k => Contextual k where
    environment  :: Lens' Context (Map.Map String (Scheme k))
    substEnv     :: Lens' Subst (Map.Map String k)

class Inferable k => Dual k where
    dual :: k -> k -> Infer Subst

data Subst = Subst
    { _sessionSubst :: Map.Map String Session
    , _typeSubst    :: Map.Map String Type
    , _nameSubst    :: Map.Map String Name }
    deriving (Show)
makeLenses ''Subst

singleSubst :: forall k. Contextual k => String -> k -> Subst
singleSubst s k = mempty & substEnv @k .~ Map.singleton s k

singletonFreeVar :: forall proxy k. Inferable k => proxy k -> String -> FreeVars
singletonFreeVar p n = mempty & freeVarsSet p .~ Set.singleton n

instance PrettyShow t => PrettyShow (Scheme t) where
    prettyShow (Scheme (FreeVars sn tn fn) t)
        | null sn && null tn             = prettyShow t
        | null sn && not (null tn)       = prettyShow t <+> "where" <+> showVars tn "Type"
        | not (null sn) && null tn       = prettyShow t <+> "where" <+> showVars sn "Session"
        | not (null sn) && not (null tn) = prettyShow t <+> "where" <+> showVars sn "Session" <> "," <+> showVars tn "Type"
        where showVars ns ks = cat ((punctuate ", ") (map text (Set.toList ns))) <> ":" <+> ks

instance Semigroup Subst where
    s@(Subst ss1 st1 sn1) <> Subst ss2 st2 sn2 = Subst
        { _sessionSubst = fmap (substitute s) ss2 <> ss1
        , _typeSubst    = fmap (substitute s) st2 <> st1
        , _nameSubst    = fmap (substitute s) sn2 <> sn1 }

instance Monoid Subst where
    mempty = Subst mempty mempty mempty
    mappend = (<>)

instance Semigroup FreeVars where
    FreeVars sv1 tv1 fn1 <> FreeVars sv2 tv2 fn2 = FreeVars (sv1 <> sv2) (tv1 <> tv2) (fn1 <> fn2)

instance Monoid FreeVars where
    mempty = FreeVars mempty mempty mempty
    mappend = (<>)

defaultInferState :: InferState
defaultInferState = InferState
    { _typeVarSupply    = 0
    , _sessionVarSupply = 0
    , _nameSupply       = 0 }

runInfer :: Infer a -> Context -> Either InferError a
runInfer (Infer rse) ctx = runExcept (evalStateT (runReaderT rse ctx) defaultInferState)

resolve :: Contextual k => String -> Infer (Scheme k)
resolve n = Map.lookup n <$> view environment >>= \case
    Just t  -> pure t
    Nothing -> throwError (InferError $ "Kind variable" <+> backticks (text n) <+> "is not defined")

lookupTypeName :: String -> Infer (Scheme Type)
lookupTypeName n = Map.lookup n <$> view typeNames >>= \case
    Just t  -> pure t
    Nothing -> throwError (InferError $ "Type" <+> backticks (text n) <+> "is not defined")

introduce :: forall k a. Contextual k => String -> Scheme k -> Infer a -> Infer a
introduce n k = local (environment @k %~ Map.insert n k)

remove :: forall proxy k a. Contextual k => proxy k -> String -> Infer a -> Infer a
remove _ n = local (environment @k %~ Map.delete n)

removeSubst :: forall proxy k. Contextual k => proxy k -> String -> Subst -> Subst
removeSubst _ x s = s & substEnv @k %~ Map.delete x

free :: forall proxy k a. Contextual k => proxy k -> String -> Infer a -> Infer a
free _ n inf = do
    t  <- fresh @k n
    t' <- generalize t
    introduce @k n t' $ remove (Of @k) n inf

freshIdent :: forall proxy k. Inferable k => proxy k -> String -> Infer String
freshIdent p prefix = do
    i <- use (varSupply p)
    varSupply p += 1
    pure (prefix ++ show i)

fresh :: forall k. Inferable k => String -> Infer k
fresh = fmap varConstr . freshIdent (Of @k)

generalize :: forall k. Contextual k => k -> Infer (Scheme k)
generalize t = do
    env <- view (environment @k)
    pure (Scheme (freeVars t \\ freeVars env) t)

instantiate :: forall k. Kind k => Scheme k -> Infer k
instantiate (Scheme (FreeVars sn tn fn) t) = do
    svs <- forM (Set.toList sn) $ \ n -> (n,) <$> fresh @Session n
    tvs <- forM (Set.toList tn) $ \ n -> (n,) <$> fresh @Type n
    fns <- forM (Set.toList fn) $ \ n -> (n,) <$> fresh @Name n
    let s = Subst (Map.fromList svs) (Map.fromList tvs) (Map.fromList fns)
    pure (substitute s t)

bindVar :: forall k. (Eq k, PrettyShow k, Contextual k) => String -> k -> Infer Subst
bindVar u t
    | t == varConstr u     = pure mempty
    | u `Set.member` fkv t = throwError (InferError $ "Occur check fails:" <+> text u <+> "~" <+> prettyShow t)
    | otherwise            = pure (mempty & substEnv .~ Map.singleton u t)
    where fkv = view (freeVarsSet (Of @k))  . freeVars

substituteGlobal :: Subst -> Infer a -> Infer a
substituteGlobal s = local ((typeEnv %~ substitute s) . (sessionEnv %~ substitute s))

instance Kind Process where
    freeVars = \case
        InputP x y p    -> freeVars x <> (freeVars p \\ singletonFreeVar Name' y)
        OutputP x y     -> freeVars x <> freeVars y
        NewP x mt p     -> (freeVars p \\ singletonFreeVar Name' x) <> foldMap freeVars mt
        ParP p q        -> freeVars p <> freeVars q
        SelectP x ps    -> freeVars x <> foldMap freeVars ps
        ProcP x mt ms p -> (freeVars p \\ singletonFreeVar Name' x)
                        <> (foldMap freeVars ms \\ singletonFreeVar Name' x)
                        <> foldMap freeVars mt
        CallP p a       -> freeVars p <> freeVars a
        VarP n          -> mempty
        DropP x         -> freeVars x
        AnnP p s        -> freeVars p <> freeVars s
        NilP            -> mempty

    substitute s@(Subst ss st sn) = \case
        InputP x y p    -> InputP (substitute s x) y (substitute (removeSubst Name' y s) p)
        OutputP x y     -> OutputP (substitute s x) (substitute s y)
        NewP x mt p     -> NewP x (substitute s <$> mt) (substitute (removeSubst Name' x s) p)
        ParP p q        -> (ParP `on` substitute s) p q
        SelectP x ps    -> SelectP (substitute s x) (substitute s <$> ps)
        ProcP x mt ms p -> ProcP x (substitute s' <$> mt) (substitute s' <$> ms) (substitute s' p)
            where s' = removeSubst Name' x s
        CallP p a       -> CallP (substitute s p) (substitute s a)
        VarP n          -> VarP n
        DropP x         -> DropP (substitute (Subst ss st Map.empty) x)
        AnnP p k        -> AnnP (substitute s p) (substitute s k)
        NilP            -> NilP

instance Kind Record where
    freeVars = \case
        SumR l n     -> freeVars n
        ProdR fs ext -> foldMap freeVars (Compose fs) <> maybe mempty (singletonFreeVar RecordType') ext
        EmptyR       -> mempty

    substitute s@(Subst _ st _) (ProdR fs ext) = ProdR (fmap (substitute s) <$> fs) ext
    substitute s (SumR l x) = SumR l (substitute s x)
    substitute s EmptyR = EmptyR

instance Inferable Record where
    type Expr Record = Record

    freeVarsSet _ = freeNames
    varSupply _   = nameSupply
    varConstr     = ProdR [] . Just

    unify r1 r2 = case (r1, r2) of
        (SumR l x, SumR l' x')
            | l == l' -> unify x x'
        (ProdR fs ext, ProdR fs' ext') -> undefined
        _ -> throwError (InferError $ "Cannot unify records" <+> backticks (prettyShow r1) <+> "and" <+> backticks (prettyShow r2))

    infer = undefined

instance Kind Name where
    freeVars = \case
        VarN n     -> singletonFreeVar Name' n
        FieldN n _ -> freeVars n
        RecN r     -> freeVars r
        QuoteN p   -> mempty
        LitN l     -> mempty

    substitute s@(Subst _ _ sn) (VarN n) = case Map.lookup n sn of
        Just n' -> n'
        Nothing -> VarN n
    substitute s (FieldN x f) = FieldN (substitute s x) f
    substitute s (RecN r) = RecN (substitute s r)
    substitute (Subst ss st _) (QuoteN p) = QuoteN (substitute (Subst ss st mempty) p)
    substitute _ x = x

instance Inferable Name where
    type Expr Name = Name

    freeVarsSet _ = freeNames
    varSupply _   = nameSupply
    varConstr     = VarN

    unify n1 n2 = case (n1, n2) of
        (VarN n, x) -> bindVar n x
        (x, VarN n) -> bindVar n x
        (FieldN x f, FieldN x' f') | f == f' -> unify x x'
        (RecN r, RecN r') -> unify r r'
        _ -> throwError (InferError $ "Cannot unify names" <+> backticks (prettyShow n1) <+> "and" <+> backticks (prettyShow n2))
    
    infer = undefined

instance Contextual Name where
    environment = nameBindings
    substEnv = nameSubst

instance Kind Type where
    freeVars = \case
        VarT n    -> singletonFreeVar Type' n
        RecordT r -> freeVars r
        QuoteT s  -> freeVars s
        _         -> mempty

    substitute s@(Subst _ st _) (VarT n) = case Map.lookup n st of
        Just t  -> t
        Nothing -> VarT n
    substitute s (RecordT r) = RecordT (substitute s r)
    substitute s (QuoteT s') = QuoteT (substitute s s')
    substitute _ t = t

instance Kind Field where
    freeVars (Field _ t) = freeVars t
    substitute s (Field l t) = Field l (substitute s t)

instance Kind RecordType where
    freeVars = \case
        VarRT n    -> singletonFreeVar Type' n
        EmptyRT    -> mempty
        ProdRT f r -> freeVars f <> freeVars r
        SumRT f r  -> freeVars f <> freeVars r

    substitute s@(Subst _ st _) = \case
        VarRT n -> case Map.lookup n st of
            Just (RecordT rt) -> substitute s rt
            _                 -> VarRT n
        SumRT f r  -> SumRT (substitute s f) (substitute s r)
        ProdRT f r -> ProdRT (substitute s f) (substitute s r)
        EmptyRT    -> EmptyRT

instance Inferable RecordType where
    type Expr RecordType = Record

    freeVarsSet _ = freeTypeVars
    varSupply _   = typeVarSupply
    varConstr     = VarRT

    unify r r' = case (r, r') of
        (EmptyRT, EmptyRT) -> pure mempty
        (VarRT n, t) -> bindVar n (RecordT t)
        (t, VarRT n) -> bindVar n (RecordT t)
        (SumRT{}, SumRT{}) -> unifyConstr SumRT r r'
        (ProdRT{}, ProdRT{}) -> unifyConstr ProdRT r r'
        _ -> err
      where err = throwError (InferError $ "Cannot unify record types" <+> backticks (prettyShow r) <+> "and" <+> backticks (prettyShow r'))
            

    infer (SumR l x) = do
        (s, t) <- infer x
        ext <- fresh @RecordType "r"
        pure (s, SumRT (Field l t) ext)
    infer (ProdR [] ext) = pure (mempty, maybe EmptyRT VarRT ext)
    infer (ProdR ((f, x) : fs) ext) = do
        (s1, t) <- infer x
        r <- fresh @RecordType "r"
        (s2, rest) <- substituteGlobal s1 (infer (ProdR fs ext))
        pure (s2 <> s1, ProdRT (Field f t) rest)
    infer EmptyR = pure (mempty, EmptyRT)

unifyConstr :: (Field -> RecordType -> RecordType) -> RecordType -> RecordType -> Infer Subst
unifyConstr constr r r' = case (ext, ext') of
    (Nothing, Nothing)
        | ((==) `on` Map.keysSet) fs fs' -> unifyFields inner inner'
        | otherwise -> err
    (Just extVar, Nothing) -> do
        s1 <- unifyFields fs inner' <|> err
        nExt' <- freshIdent RecordType' "r"
        s2 <- bindVar extVar (RecordT $ mapToRecordType constr outer' (Just nExt'))
        pure (s2 <> s1)
    (Nothing, Just extVar') -> do
        s1 <- unifyFields inner fs' <|> err
        nExt <- freshIdent RecordType' "r"
        s2 <- bindVar extVar' (RecordT $ mapToRecordType constr outer (Just nExt))
        pure (s2 <> s1)
    (Just extVar, Just extVar') -> do
        s1 <- unifyFields inner fs' <|> err
        s2 <- (unifyFields `on` substitute s1) fs inner' <|> err
        (nExt, nExt') <- (,) <$> freshIdent RecordType' "r" <*> freshIdent RecordType' "r"
        s3 <- bindVar extVar (substitute (s2 <> s1) (RecordT $ mapToRecordType constr outer' (Just nExt')))
        s4 <- bindVar extVar' (substitute (s3 <> s2 <> s1) (RecordT $ mapToRecordType constr outer (Just nExt)))
        pure (s4 <> s3 <> s2 <> s1)
  where unifyFields x y = foldr (\ (t, t') ms -> ms >>= \ s -> unifyWithSubst s t t') (pure mempty) (Map.intersectionWith (,) x y)
        ((fs, ext), (fs', ext')) = ((,) `on` mapFromRecordType) r r'
        zipMap k x y     = Just (x, y)
        (inner, outer)   = (Map.restrictKeys fs (Map.keysSet fs'), Map.restrictKeys fs ((Set.difference `on` Map.keysSet) fs fs'))
        (inner', outer') = (Map.restrictKeys fs' (Map.keysSet fs), Map.restrictKeys fs' ((Set.difference `on` Map.keysSet) fs' fs))
        err = throwError (InferError $ "Cannot unify record types" <+> backticks (prettyShow r) <+> "and" <+> backticks (prettyShow r'))

instance Substructural RecordType where
    substruct rt1 rt2 = flip (<|>) err $ case (rt1, rt2) of
        (SumRT{}, SumRT{}) -> substructConstr SumRT rt1 rt2
        (ProdRT{}, ProdRT{}) -> substructConstr ProdRT rt1 rt2
        _ -> unify rt1 rt2
      where err = throwError (InferError $ "Record type" <+> backticks (prettyShow rt1) <+> "is not a sub-record of" <+> backticks (prettyShow rt2))

substructConstr :: (Field -> RecordType -> RecordType) -> RecordType -> RecordType -> Infer Subst
substructConstr constr r r' = case (ext, ext') of
    (Nothing, Nothing)
        | ((==) `on` Map.keysSet) fs fs' -> substructFields inner inner'
        | otherwise -> err
    (Just extVar, Nothing) -> do
        s1 <- substructFields fs inner' <|> err
        nExt' <- freshIdent RecordType' "r"
        s2 <- bindVar extVar (RecordT $ mapToRecordType constr outer' (Just nExt'))
        pure (s2 <> s1)
    (Nothing, Just extVar') -> do
        s1 <- substructFields inner fs' <|> err
        nExt <- freshIdent RecordType' "r"
        s2 <- bindVar extVar' (RecordT $ mapToRecordType constr outer (Just nExt))
        pure (s2 <> s1)
    (Just extVar, Just extVar') -> do
        s1 <- substructFields inner fs' <|> err
        s2 <- (substructFields `on` substitute s1) fs inner' <|> err
        (nExt, nExt') <- (,) <$> freshIdent RecordType' "r" <*> freshIdent RecordType' "r"
        s3 <- bindVar extVar (substitute (s2 <> s1) (RecordT $ mapToRecordType constr outer' (Just nExt')))
        s4 <- bindVar extVar' (substitute (s3 <> s2 <> s1) (RecordT $ mapToRecordType constr outer (Just nExt)))
        pure (s4 <> s3 <> s2 <> s1)
  where substructFields x y = foldr (\ (t, t') ms -> ms >>= \ s -> substructWithSubst s t t') (pure mempty) (Map.intersectionWith (,) x y)
        ((fs, ext), (fs', ext')) = ((,) `on` mapFromRecordType) r r'
        zipMap k x y     = Just (x, y)
        (inner, outer)   = (Map.restrictKeys fs (Map.keysSet fs'), Map.restrictKeys fs ((Set.difference `on` Map.keysSet) fs fs'))
        (inner', outer') = (Map.restrictKeys fs' (Map.keysSet fs), Map.restrictKeys fs' ((Set.difference `on` Map.keysSet) fs' fs))
        err = throwError (InferError $ "Record type" <+> backticks (prettyShow r) <+> "is not a sub-record of" <+> backticks (prettyShow r'))

instance Inferable Type where
    type Expr Type = Name

    freeVarsSet _ = freeTypeVars
    varSupply _   = typeVarSupply
    varConstr     = VarT

    unify (VarT u) t = bindVar u t
    unify t (VarT u) = bindVar u t
    unify (RecordT r) (RecordT r') = unify r r'
    unify (QuoteT s) (QuoteT s') = unify s s'
    unify t t'
        | t == t'   = pure mempty 
        | otherwise = throwError (InferError $ "Types" <+> backticks (prettyShow t) <+> "and" <+> backticks (prettyShow t') <+> "do not unify")

    infer (VarN n) = do
        sigma <- resolve n
        t <- instantiate sigma
        pure (mempty, t)
    infer (LitN l) = pure (mempty, inferLit l)
    infer (RecN r) = do
        (s, rt) <- infer r
        pure (s, substitute s (RecordT rt))
    infer (QuoteN p) = do
        (s, k) <- infer p
        pure (s, QuoteT (substitute s k))
    infer (FieldN x f) = do
        (s1, r) <- infer x
        ft <- fresh @Type "f"
        ext <- fresh @RecordType "r"
        s2 <- unifyWithSubst s1 r (RecordT (SumRT (Field f ft) ext))
            <|> throwError (InferError $ "Unable to get field" <+> backticks (text f) <+> "of record" <+> backticks (prettyShow r))
        let s = s2 <> s1
            t = substitute s ft
        pure (s, t)

instance Substructural Type where
    substruct type1 type2 = case (type1, type2) of
        (RecordT r, RecordT r') -> substruct r r'
        (QuoteT s, QuoteT s') -> substruct s s'
        _ -> unify type1 type2 <|> err
      where err = throwError (InferError $ "Type" <+> backticks (prettyShow type1) <+> "is not a sub-type of" <+> backticks (prettyShow type2))

instance Contextual Type where
    environment = typeEnv
    substEnv    = typeSubst

instance Kind Session where
    freeVars = \case
        VarS n      -> singletonFreeVar Session' n
        ReadS _ s   -> freeVars s
        WriteS _    -> mempty
        ParS s s'   -> freeVars s <> freeVars s'
        ProcS n t s -> freeVars t <> (freeVars s \\ singletonFreeVar Name' n)
        NilS        -> mempty

    substitute s@(Subst ss _ _) (VarS n) = case Map.lookup n ss of
        Just k  -> substitute s k
        Nothing -> VarS n
    substitute s (ReadS x r)   = ReadS (substitute s x) (substitute s r)
    substitute s (WriteS x)    = WriteS (substitute s x)
    substitute s (ParS r r')   = (ParS `on` substitute s) r r'
    substitute s (ProcS x t r) = ProcS x (substitute s t) (substitute (removeSubst Name' x s) r)
    substitute s NilS          = NilS

instance Inferable Session where
    type Expr Session = Process

    freeVarsSet _ = freeSessionVars
    varSupply _   = sessionVarSupply
    varConstr     = VarS

    unify (VarS u) t = bindVar u t
    unify t (VarS u) = bindVar u t
    unify (ReadS x s) (ReadS x' s')
        | x `nameEq` x' = pure mempty
    unify (WriteS x) (WriteS x')
        | x `nameEq` x' = pure mempty
    unify (ProcS x t s) (ProcS x' t' s') = do
        s1 <- unify t t'
        s2 <- unifyWithSubst s1 (substitute (singleSubst x (VarN x')) s) s'
        pure (s1 <> s2)
    unify NilS NilS = pure mempty
    unify s@ParS{} s'@ParS{} =
        let ps = eventPairs s s'
            unifyPermutations xs ys = foldr (\ (si, sj) ms -> ms >>= \ s -> unifyWithSubst s si sj) (pure mempty) (zip xs ys)
        in foldr (<|>)
            (throwError (InferError $ "Sessions" <+> backticks (prettyShow s) <+> "and" <+> backticks (prettyShow s') <+> "do not unify"))
            (map (uncurry unifyPermutations) ps)
    unify s s' = throwError (InferError $ "Sessions" <+> backticks (prettyShow s) <+> "and" <+> backticks (prettyShow s') <+> "do not unify")

    infer (InputP x y p) = do
        (s1, tx) <- infer x
        ty <- fresh @Type "a"
        (s2, k) <- substituteGlobal s1 $ introduce y (Scheme mempty ty) (infer p)
        let s = s2 <> s1
        s3 <- unifyWithSubst s tx ty
        pure (s3 <> s, substitute (s3 <> s) $ ReadS x (privatize (VarN y) k))
    infer (OutputP x y) = do
        (s1, tx) <- infer x
        (s2, ty) <- infer y
        let s = s2 <> s1
        s3 <- unifyWithSubst @Type s tx ty
        pure (s3 <> s, WriteS x)
    infer (NewP n (Just t) r) = do
        (s, k) <- introduce n (Scheme mempty t) (infer r)
        pure (s, privatize (VarN n) (substitute s k))
    infer (NewP n Nothing r) = do
        t <- fresh @Type "a"
        (s, k) <- introduce n (Scheme mempty t) (infer r)
        pure (s, privatize (VarN n) (substitute s k))
    infer (ProcP x Nothing Nothing p) = do
        t <- fresh @Type "a"
        (s, k) <- introduce x (Scheme mempty t) (infer p)
        pure (s, ProcS x (substitute s t) (substitute s k))
    infer (ProcP x (Just t) Nothing p) = do
        (s, k) <- introduce x (Scheme mempty t) (infer p)
        pure (s, ProcS x t (substitute s k))
    infer (ProcP x Nothing (Just s) p) = do
        t <- fresh @Type "a"
        (s1, s') <- introduce x (Scheme mempty t) (infer p)
        s2 <- unifyWithSubst s1 s s'
        let subst = s2 <> s1
        pure (subst, ProcS x (substitute subst t) (substitute subst s))
    infer (ProcP x (Just t) (Just s) p) = do
        (s1, s') <- introduce x (Scheme mempty t) (infer p)
        s2 <- unifyWithSubst s1 s s'
        pure (s2 <> s1, ProcS x t s)
    infer (CallP p a) = do
        (s1, t) <- infer a
        (s2, s) <- substituteGlobal s1 (infer p)
        x <- freshIdent Name' "$"
        r <- fresh @Session "s"
        s3 <- unifyWithSubst (s2 <> s1) s (ProcS x t r)
        let subst = singleSubst x a <> s3 <> s2 <> s1
            r' = substitute subst r
        pure . (subst,) $ if constantName a
            then privatize a r'
            else r'
        -- throwError (InferError $ "Process" <+> backticks (prettyShow p) <+> "is not callable")
    infer (DropP x) = do
        (s1, t) <- infer x
        k <- fresh @Session "s"
        s2 <- unifyWithSubst s1 t (QuoteT k)
        let s = s2 <> s1
        pure (s, substitute s k)
    infer (ParP p q) = do
        (s1, sp) <- infer p
        (s2, sq) <- substituteGlobal s1 (infer q)
        let s = s2 <> s1
        pure (s, (ParS `on` substitute s) sp sq)
    infer (SelectP x ps) = do
        ss <- replicateM (length ps) (fresh @Session "s")
        -- (s1, ss) <- (_1 %~ fold) . unzip <$> mapM infer ps
        (s1, ss') <- foldr (\ p m -> m >>= \ (sb, ss) -> (_1 %~ (sb <>)) . (_2 %~ (:ss)) <$> substituteGlobal sb (infer p)) (pure (mempty, [])) ps
        -- let expected = ReadS x s
        s2 <- foldr (\ (s, s') m -> m >>= \ sb -> (sb <>) <$> unifyWithSubst sb s s') (pure s1) (zip ss ss')
        (s3, ks) <- exhaustiveRead x ss'
        let subst = s3 <> s2 <> s1
        pure (subst, ReadS x (substitute subst (foldr ParS NilS ks)))
    infer (VarP n) = do
        sigma <- resolve n
        s <- instantiate sigma
        pure (mempty, s)
    infer (AnnP p s) = do
        (s1, s') <- infer p
        s2 <- unifyWithSubst s1 s s'
        pure (s2 <> s1, s)
    infer NilP = pure (mempty, NilS)

exhaustiveRead :: Name -> [Session] -> Infer (Subst, [Session])
exhaustiveRead x ss = do
    (s1, t) <- infer x
    (s2, ifs) <- getInputFields x ss
    let pats = generatePatterns x t
        (unmatched, ss') = foldr (\ (p, k) (ms, ks) -> (Set.filter (not . flip isSubName p) ms, k:ks)) (pats, []) ifs
    if Set.null unmatched
        then pure (s2 <> s1, ss')
        else throwError (InferError $ "Non exhaustive read when selecting name" <+> backticks (prettyShow x) <> ", missing" <+> cat (punctuate ", " (map (backticks . prettyShow) (Set.toList unmatched))))

getInputFields :: Name -> [Session] -> Infer (Subst, Set.Set (Name, Session))
getInputFields _ [] = pure (mempty, Set.empty)
getInputFields x (s:ss) = do
    k <- fresh @Session "s"
    s1 <- substruct s (ReadS x k)
    (s2, ifs) <- getInputFields x ss
    let subst = s2 <> s1
        (s', k') = ((,) `on` substitute subst) s k
    pure . (subst,) $ case s' of
        ReadS y k'' -> Set.singleton (y, k'')
        _ -> Set.empty
-- getInputFields (s:ss) = traceShow s $ getInputFields ss <> case s of
--     ReadS y s -> do
--         getInputFields ss
--         Set.singleton (y, s)
--     _         -> getInputFields ss

generatePatterns :: Name -> Type -> Set.Set Name
generatePatterns x = go mempty
    where go acc = (acc <>) . \case
              RecordT rt@SumRT{} ->
                let (fs, ext) = mapFromRecordType rt
                in  foldMap (\ (f, t) -> generatePatterns (FieldN x f) t) (Map.assocs fs)
                    -- <> foldMap (Set.singleton x) ext
              t -> Set.singleton x

instance Substructural Session where
    substruct sess1 sess2 = case (sess1, sess2) of
        (ReadS x s, ReadS x' s')
            | x' `isSubName` x -> substruct s s' <|> err s s'
            | otherwise        -> err sess1 sess2
        (WriteS x, WriteS x')
            | x `isSubName` x' -> pure mempty
            | otherwise        -> err sess1 sess2
        (ParS{}, ParS{}) ->
            let ps = eventPairs sess1 sess2
                substructPermutations xs ys = foldr (\ (si, sj) ms -> ms >>= \ s -> substructWithSubst s si sj) (pure mempty) (zip xs ys)
            in foldr (<|>)
                (err sess1 sess2)
                (map (uncurry substructPermutations) ps)
        (ProcS x t s, ProcS x' t' s') -> do
            s1 <- substruct t t'
            s2 <- substructWithSubst s1 s (substitute (singleSubst x' (VarN x)) s') <|> err s s'
            pure (removeSubst Name' x' s2 <> s1)
        (NilS, _) -> pure mempty
        _ -> unify sess1 sess2 <|> err sess1 sess2
      where err s1 s2 = throwError (InferError $ "Session" <+> backticks (prettyShow s1) <+> "is not a sub-session of" <+> backticks (prettyShow s2))

instance Contextual Session where
    environment = sessionEnv
    substEnv    = sessionSubst

instance Dual Session where
    dual s s' = undefined

normalizeParS :: Session -> [Session]
normalizeParS (ParS s s') = ((++) `on` normalizeParS) s s'
normalizeParS NilS = []
normalizeParS s = [s]

eventPairs :: Session -> Session -> [([Session], [Session])]
eventPairs s1 s2
    | length ns1 < length ns2 = zip (permutations (ns1 ++ replicate (length ns2 - length ns1) NilS)) (permutations ns2)
    | otherwise               = zip (permutations ns1) (permutations (ns2 ++ replicate (length ns1 - length ns2) NilS))
    where (ns1, ns2) = ((,) `on` normalizeParS) s1 s2

nameEq :: Name -> Name -> Bool
nameEq QuoteN{} _ = False
nameEq _ QuoteN{} = False
nameEq x y = x == y

inferLit :: Literal -> Type
inferLit IntL{} = IntT
inferLit FloatL{} = FloatT
inferLit CharL{} = CharT
inferLit StringL{} = StringT

isSubName :: Name -> Name -> Bool
x `isSubName` FieldN y f = x `isSubName` y
x `isSubName` y = x == y

constantName :: Name -> Bool
constantName x = Set.null (freeVars x ^. freeNames)

privatize :: Name -> Session -> Session
privatize n (ReadS x s)
    | n `isSubName` x = privatize x s
    | otherwise       = ReadS x (privatize n s)
privatize n (WriteS x)
    | n `isSubName` x = NilS
    | otherwise       = WriteS x
privatize n (ParS s s') = (ParS `on` privatize n) s s'
privatize n (VarS s) = VarS s
privatize n (ProcS n' t s)
    | n == VarN n' = ProcS n' t s
    | otherwise    = ProcS n' t (privatize n s)
privatize _ NilS = NilS

inferProcess :: Process -> Infer (Scheme Session)
inferProcess p = do
    (s, k) <- infer p
    generalize (substitute s k)
