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
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  #-}

module Language.Lys.Core where

import Language.Lys.Pretty
import Language.Lys.Types

import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

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
    Type'    :: KindProxy Type
    Session' :: KindProxy Session
    Of       :: forall k. KindProxy k

data FreeVars = FreeVars
    { _freeSessionVars :: Set.Set String
    , _freeTypeVars    :: Set.Set String }
makeLenses ''FreeVars

(\\) :: FreeVars -> FreeVars -> FreeVars
FreeVars sv1 tv1 \\ FreeVars sv2 tv2 = FreeVars (Set.difference sv1 sv2) (Set.difference tv1 tv2)

data Scheme k = Scheme
    { _boundVars  :: FreeVars
    , _schemeKind :: k }
    deriving (Functor, Foldable)
makeLenses ''Scheme

singletonTV, singletonSV :: String -> FreeVars
singletonTV n = FreeVars Set.empty (Set.singleton n)
singletonSV n = FreeVars (Set.singleton n) Set.empty

data Context = Context
    { _typeNames   :: Map.Map String (Scheme Type)
    , _typeEnv     :: Map.Map String (Scheme Type)
    , _sessionEnv  :: Map.Map String (Scheme Session) }
makeLenses ''Context

data InferState = InferState
    { _typeVarSupply    :: Int
    , _sessionVarSupply :: Int
    , _paramSupply      :: Int }
makeLenses ''InferState

data InferError = InferError Doc
    deriving Show

class Kind k where
    freeVars   :: k -> FreeVars
    substitute :: Subst -> k -> k

instance (Foldable f, Functor f, Kind k) => Kind (f k) where
    freeVars = foldMap freeVars
    substitute   = fmap . substitute
    
class Kind k => Inferable k where
    environment  :: Lens' Context (Map.Map String (Scheme k))
    freeVarsSet  :: proxy k -> Lens' FreeVars (Set.Set String)
    substEnv     :: Lens' Subst (Map.Map String k)
    varSupply    :: proxy k -> Lens' InferState Int
    varConstr    :: String -> k

data Subst = Subst
    { _sessionSubst :: Map.Map String Session
    , _typeSubst    :: Map.Map String Type }
makeLenses ''Subst

instance PrettyShow t => PrettyShow (Scheme t) where
    prettyShow (Scheme (FreeVars sn tn) t)
        | null sn && null tn             = prettyShow t
        | null sn && not (null tn)       = prettyShow t <+> "where" <+> showVars tn "Type"
        | not (null sn) && null tn       = prettyShow t <+> "where" <+> showVars sn "Session"
        | not (null sn) && not (null tn) = prettyShow t <+> "where" <+> showVars sn "Session" <> "," <+> showVars tn "Type"
        where showVars ns ks = cat ((punctuate ", ") (map text (Set.toList ns))) <> ":" <+> ks

instance Semigroup Subst where
    s@(Subst ss1 st1) <> Subst ss2 st2 = Subst
        { _sessionSubst = fmap (substitute s) ss2 <> ss1
        , _typeSubst    = fmap (substitute s) st2 <> st1 }

instance Monoid Subst where
    mempty = Subst mempty mempty
    mappend = (<>)

instance Semigroup FreeVars where
    FreeVars sv1 tv1 <> FreeVars sv2 tv2 = FreeVars (sv1 <> sv2) (tv1 <> tv2)

instance Monoid FreeVars where
    mempty = FreeVars mempty mempty
    mappend = (<>)

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

defaultInferState :: InferState
defaultInferState = InferState
    { _typeVarSupply    = 0
    , _sessionVarSupply = 0
    , _paramSupply      = 0 }

runInfer :: Infer a -> Context -> Either InferError a
runInfer (Infer rse) ctx = runExcept (evalStateT (runReaderT rse ctx) defaultInferState)

resolve :: Inferable k => String -> Infer (Scheme k)
resolve n = Map.lookup n <$> view environment >>= \case
    Just t  -> return t
    Nothing -> throwError (InferError $ "Type variable" <+> backticks (text n) <+> "is not defined")

lookupTypeName :: String -> Infer (Scheme Type)
lookupTypeName n = Map.lookup n <$> view typeNames >>= \case
    Just t  -> return t
    Nothing -> throwError (InferError $ "Type" <+> backticks (text n) <+> "is not defined")

introduce :: forall k a. Inferable k => String -> Scheme k -> Infer a -> Infer a
introduce n k = local (environment @k %~ Map.insert n k)

freshParam :: Infer String
freshParam = do
    i <- use paramSupply
    let n = "$" ++ show i
    paramSupply += 1
    pure n

remove :: forall proxy k a. Inferable k => proxy k -> String -> Infer a -> Infer a
remove _ n = local (environment @k %~ Map.delete n)

free :: forall proxy k a. Inferable k => proxy k -> String -> Infer a -> Infer a
free _ n inf = do
    t  <- fresh @k n
    t' <- generalize t
    introduce @k n t' $ remove (Proxy :: Proxy k) n inf

fresh :: forall k. Inferable k => String -> Infer k
fresh prefix = do
    i <- use (varSupply (Of @k))
    varSupply (Of @k) += 1
    return . varConstr $ prefix ++ show i

generalize :: forall k. Inferable k => k -> Infer (Scheme k)
generalize t = do
    env <- view (environment @k)
    return (Scheme (freeVars t \\ freeVars env) t)

instantiate :: forall k. Kind k => Scheme k -> Infer k
instantiate (Scheme (FreeVars sn tn) t) = do
    svs <- forM (Set.toList sn) $ \ n -> (n,) <$> fresh @Session n
    tvs <- forM (Set.toList tn) $ \ n -> (n,) <$> fresh @Type n
    let s = Subst (Map.fromList svs) (Map.fromList tvs) 
    pure (substitute s t)

bindVar :: forall k. (Eq k, PrettyShow k, Inferable k) => String -> k -> Infer Subst
bindVar u t
    | t == varConstr u     = pure mempty
    | u `Set.member` fkv t = throwError (InferError $ "Occur check fails:" <+> text u <+> "~" <+> prettyShow t)
    | otherwise            = pure (mempty & substEnv .~ Map.singleton u t)
    where fkv = view (freeVarsSet (Of @k))  . freeVars

substituteGlobal :: Subst -> Infer a -> Infer a
substituteGlobal s = local ((typeEnv %~ substitute s) . (sessionEnv %~ substitute s))

instance Kind Name where
    freeVars = undefined

instance Kind Type where
    freeVars = \case
        VarT n        -> singletonTV n
        EmptyT        -> mempty
        ExtendT l t r -> freeVars t <> freeVars r
        RecordT r     -> freeVars r
        VariantT v    -> freeVars v
        QuoteT s      -> freeVars s
        _             -> mempty

    substitute (Subst _ st) (VarT n) = case Map.lookup n st of
        Just t  -> t
        Nothing -> VarT n
    substitute s (ExtendT l t r) = ExtendT l (substitute s t) (substitute s r)
    substitute s (RecordT r) = RecordT (substitute s r)
    substitute s (VariantT v) = VariantT (substitute s v)
    substitute s (QuoteT s') = QuoteT (substitute s s')
    substitute _ t = t

instance Inferable Type where
    environment   = typeEnv
    freeVarsSet _ = freeTypeVars
    substEnv      = typeSubst
    varSupply _   = typeVarSupply
    varConstr     = VarT

instance Kind Session where
    freeVars = \case
        VarS n      -> singletonSV n
        ReadS _ s   -> freeVars s
        WriteS _    -> mempty
        ParS s s'   -> freeVars s <> freeVars s'
        ProcS n t s -> freeVars t <> freeVars s
        NilS        -> mempty

    substitute (Subst ss st) (VarS n) = case Map.lookup n ss of
        Just s  -> substitute (Subst mempty st) s
        Nothing -> VarS n
    substitute s (ReadS x r)   = ReadS x (substitute s r)
    substitute s (WriteS x)    = WriteS x
    substitute s (ParS r r')   = (ParS `on` substitute s) r r'
    substitute s (ProcS x t r) = ProcS x (substitute s t) (substitute s r)
    substitute s NilS          = NilS

instance Inferable Session where
    environment   = sessionEnv
    freeVarsSet _ = freeSessionVars
    substEnv      = sessionSubst
    varSupply _   = sessionVarSupply
    varConstr     = VarS

rewriteRow :: Type -> Label -> Infer (Type, Type, Subst)
rewriteRow EmptyT newLabel = throwError (InferError $ "Label" <+> backticks (text newLabel) <+> "cannot be inserted")
rewriteRow (ExtendT label fieldTy rowTail) newLabel
    | newLabel == label     = return (fieldTy, rowTail, mempty)
    | VarT alpha <- rowTail = do
        beta  <- fresh "r"
        gamma <- fresh "a"
        return ( gamma
                , ExtendT label fieldTy beta
                , Subst mempty (Map.singleton alpha $ ExtendT newLabel gamma beta)
                )
    | otherwise   = do
        (fieldTy', rowTail', s) <- rewriteRow rowTail newLabel
        return ( fieldTy'
                , ExtendT label fieldTy rowTail'
                , s
                )
rewriteRow ty _ = throwError (InferError $ "Unexpected type:" <+> backticks (prettyShow ty))

unifyType :: Type -> Type -> Infer Subst
unifyType (VarT u) t = bindVar u t
unifyType t (VarT u) = bindVar u t
unifyType (RecordT row1) (RecordT row2) = unifyType row1 row2
unifyType (VariantT row1) (VariantT row2) = unifyType row1 row2
unifyType (RecordT EmptyT) EmptyT = pure mempty
unifyType (VariantT EmptyT) EmptyT = pure mempty
unifyType EmptyT (RecordT EmptyT) = pure mempty
unifyType EmptyT (VariantT EmptyT) = pure mempty
unifyType (VariantT (ExtendT l t EmptyT)) (RecordT (ExtendT l' t' EmptyT))
    | l == l' = unifyType t t'
unifyType (RecordT (ExtendT l t EmptyT)) (VariantT (ExtendT l' t' EmptyT))
    | l == l' = unifyType t t'
unifyType (RecordT EmptyT) (VariantT EmptyT) = pure mempty
unifyType EmptyT EmptyT = pure mempty
unifyType (ExtendT label1 fieldTy1 rowTail1) row2@ExtendT{} = do
  (fieldTy2, rowTail2, theta1@(Subst th1 _)) <- rewriteRow row2 label1
  -- ^ apply side-condition to ensure termination
  case snd $ recordToList rowTail1 of
    Just tv | Map.member tv th1 -> throwError (InferError "Recursive row type")
    _ -> do
      theta2 <- unifyType (substitute theta1 fieldTy1) (substitute theta1 fieldTy2)
      let s = theta2 <> theta1
      theta3 <- unifyType (substitute s rowTail1) (substitute s rowTail2)
      pure $ theta3 <> s
unifyType (QuoteT s) (QuoteT s') = do
    ss <- unifySessions s s'
    pure mempty
unifyType t t'
    | t == t'   = pure mempty 
    | otherwise = throwError (InferError $ "Types" <+> backticks (prettyShow t) <+> "and" <+> backticks (prettyShow t') <+> "do not unify")

normalizeParS :: Session -> Infer [Session]
normalizeParS (ParS s s') = (liftA2 (++) `on` normalizeParS) s s'
normalizeParS NilS = pure []
normalizeParS s = pure [s]

nameEq :: Name -> Name -> Bool
nameEq QuoteN{} _ = False
nameEq _ QuoteN{} = False
nameEq x y = x == y

unifySessions :: Session -> Session -> Infer Subst
unifySessions (VarS u) t = bindVar u t
unifySessions t (VarS u) = bindVar u t
unifySessions (ReadS x s) (ReadS x' s')
    | x `nameEq` x' = pure mempty
unifySessions (WriteS x) (WriteS x')
    | x `nameEq` x' = pure mempty
unifySessions (ProcS x t s) (ProcS x' t' s') = do
    s1 <- unifyType t t'
    s2 <- (unifySessions `on` substitute s1) (substituteNameInSession x (VarN x') s) s'
    return (s1 <> s2)
unifySessions NilS NilS = pure mempty
unifySessions s s' = do
    (ps, ps') <- (,) <$> normalizeParS s <*> normalizeParS s'
    mconcat <$> foldl (<|>)
        (throwError (InferError $ "Sessions" <+> backticks (prettyShow s) <+> "and" <+> backticks (prettyShow s') <+> "do not unify") )
        [ zipWithM unifySessions p p' | (p, p') <- (,) <$> permutations ps <*> permutations ps' ]

inferName :: Name -> Infer (Subst, Type)
inferName (VarN n) = do
    sigma <- resolve n
    t <- instantiate sigma
    return (mempty, t)
inferName (LitN l) = pure (mempty, inferLit l)
inferName (RecN fs) = inferRecord fs
inferName (QuoteN p) = do
    (s, k) <- inferProcess p
    pure (s, QuoteT (substitute s k))
inferName (FieldN x f) = do
    (st1, r) <- inferName x
    ft <- fresh @Type "f"
    fr <- fresh @Type "r"
    st2 <- (unifyType `on` substitute st1) r (RecordT (ExtendT f ft fr))
    let st = st2 <> st1
    t <- lookupRecordFieldType f (substitute st r)
    pure (st, t)

lookupRecordFieldType :: Label -> Type -> Infer Type
lookupRecordFieldType l (IdentT n) = lookupRecordFieldType l =<< instantiate =<< lookupTypeName n
lookupRecordFieldType l (VariantT v) =
    let (fields, ext)  = recordToList v
    in case Map.lookup l (Map.fromList fields) of
        Just t  -> pure t
        Nothing -> throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of variant" <+> backticks (prettyShow (VariantT v)))
lookupRecordFieldType l (RecordT r) =
    let (fields, ext)  = recordToList r
    in case Map.lookup l (Map.fromList fields) of
        Just t  -> pure t
        Nothing -> throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of record" <+> backticks (prettyShow (RecordT r)))
lookupRecordFieldType l t = throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of non-record type" <+> backticks (prettyShow t))

inferRecord :: [(Label, Name)] -> Infer (Subst, Type)
inferRecord [] = return (mempty, EmptyT)
inferRecord ((f,x):fs) = do
    (s, t)   <- inferName x
    (rs, rt) <- inferRecord fs
    return (s <> rs, ExtendT f t rt)

inferLit :: Literal -> Type
inferLit IntL{} = IntT
inferLit FloatL{} = FloatT
inferLit CharL{} = CharT
inferLit StringL{} = StringT

isSubName :: Name -> Name -> Bool
VarN n `isSubName` VarN n' = n == n'
x `isSubName` FieldN y f = x `isSubName` y
_ `isSubName` _ = False

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

substituteNameInName :: String -> Name -> Name -> Name
substituteNameInName n x (VarN n')
    | n == n'   = x
    | otherwise = VarN n'
substituteNameInName n x (FieldN x' f) = FieldN (substituteNameInName n x x') f
substituteNameInName _ _ x = x

substituteNameInSession :: String -> Name -> Session -> Session
substituteNameInSession n x (ReadS x' s) = ReadS (substituteNameInName n x x') (substituteNameInSession n x s)
substituteNameInSession n x (WriteS x') = WriteS (substituteNameInName n x x')
substituteNameInSession n x (ParS s s') = (ParS `on` substituteNameInSession n x) s s'
substituteNameInSession n x (ProcS x' t r)
    | n == x'   = ProcS x' t r
    | otherwise = ProcS x' t (substituteNameInSession n x r)
substituteNameInSession _ _ (VarS n) = VarS n
substituteNameInSession _ _ NilS = NilS

inferProcess :: Process -> Infer (Subst, Session)
inferProcess (InputP x y p) = do
    (s1, tx) <- inferName x
    t <- fresh @Type "x"
    ((s2, r), ty) <- introduce y (Scheme mempty t) ((,) <$> inferProcess p <*> (instantiate =<< resolve y))
    let s = s2 <> s1
    s3 <- (unifyType `on` substitute s) tx ty
    return (s3 <> s, ReadS x (privatize (VarN y) r))
inferProcess (OutputP x y) = do
    (s1, tx) <- inferName x
    (s2, ty) <- inferName y
    let s = s2 <> s1
    s3 <- (unifyType `on` substitute s) tx ty
    return (s3 <> s, WriteS x)
inferProcess (NewP n t r) = do
    (s, k) <- introduce n (Scheme mempty t) (inferProcess r)
    return (s, privatize (VarN n) (substitute s k))
inferProcess (NewPI n r) = do
    t <- fresh @Type "a"
    (s, k) <- introduce n (Scheme mempty t) (inferProcess r)
    return (s, privatize (VarN n) (substitute s k))
inferProcess (ProcP x t p) = do
    t' <-  fresh @Type "a"
    (s1, k) <- introduce x (Scheme mempty t') (inferProcess p)
    s2 <- (unifyType `on` substitute s1) t t'
    let s = s2 <> s1
    return (s, substitute s (ProcS x t k))
inferProcess (ProcPI x p) = do
    t <- fresh @Type "a"
    (s, k) <- introduce x (Scheme mempty t) (inferProcess p)
    return (s, ProcS x (substitute s t) (substitute s k))
inferProcess (CallP p a) = do
    (s1, k) <- inferProcess p
    (s2, t) <- substituteGlobal s1 (inferName a)
    k' <- fresh @Session "s"
    n <- freshParam
    s3 <- introduce n (Scheme mempty t) $ unifySessions (substitute s2 k) (ProcS n t k')
    let s = s3 <> s2 <> s1
    return (s, substituteNameInSession n a (substitute s k'))
    -- _ -> throwError (InferError $ "Process" <+> backticks (prettyShow p) <+> "is not callable")
inferProcess (DropP x) = do
    (s1, t) <- inferName x
    k <- fresh @Session "s"
    s2 <- (unifyType `on` substitute s1) t (QuoteT k)
    let s = s2 <> s1
    return (s, substitute s k)
inferProcess (ParP p q) = do
    (s1, sp) <- inferProcess p
    (s2, sq) <- inferProcess q
    let s = s2 <> s1
    return (s, (ParS `on` substitute s) sp sq)
inferProcess (VarP n) = do
    sigma <- resolve n
    s <- instantiate sigma
    return (mempty, s)
inferProcess NilP = return (mempty, NilS)

inferProcess' :: Process -> Infer (Scheme Session)
inferProcess' p = do
    (s, k) <- inferProcess p
    generalize (substitute s k)

-- data Session
--     = ReadS String Session
--     | WriteS String Name
--     | ParS Session Session
--     | ProcS Params Session
--     | NilS
--     | VarS String
--     deriving (Eq, Ord, Show)

-- data Process
--     = InputP  Name String Process
--     | OutputP Name Name
--     | NewP String Type Process
--     | ParP Process Process
--     | ProcP Params Process
--     | NilP
--     deriving (Eq, Ord, Show)