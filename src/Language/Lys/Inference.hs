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
    Type'    :: KindProxy Type
    Session' :: KindProxy Session
    Of       :: forall k. KindProxy k

data FreeVars = FreeVars
    { _freeSessionVars :: Set.Set String
    , _freeTypeVars    :: Set.Set String }
    deriving (Show)
makeLenses ''FreeVars

(\\) :: FreeVars -> FreeVars -> FreeVars
FreeVars sv1 tv1 \\ FreeVars sv2 tv2 = FreeVars (Set.difference sv1 sv2) (Set.difference tv1 tv2)

data Scheme k = Scheme
    { _boundVars  :: FreeVars
    , _schemeKind :: k }
    deriving (Show, Functor, Foldable)
makeLenses ''Scheme

singletonTV, singletonSV :: String -> FreeVars
singletonTV n = FreeVars Set.empty (Set.singleton n)
singletonSV n = FreeVars (Set.singleton n) Set.empty

data Context = Context
    { _typeNames   :: Map.Map String (Scheme Type)
    , _typeEnv     :: Map.Map String (Scheme Type)
    , _sessionEnv  :: Map.Map String (Scheme Session) }
    deriving (Show)
makeLenses ''Context

data InferState = InferState
    { _typeVarSupply    :: Int
    , _sessionVarSupply :: Int
    , _paramSupply      :: Int }
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

class Inferable k => Contextual k where
    environment  :: Lens' Context (Map.Map String (Scheme k))
    substEnv     :: Lens' Subst (Map.Map String k)

data Subst = Subst
    { _sessionSubst :: Map.Map String Session
    , _typeSubst    :: Map.Map String Type }
    deriving (Show)
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

defaultInferState :: InferState
defaultInferState = InferState
    { _typeVarSupply    = 0
    , _sessionVarSupply = 0
    , _paramSupply      = 0 }

runInfer :: Infer a -> Context -> Either InferError a
runInfer (Infer rse) ctx = runExcept (evalStateT (runReaderT rse ctx) defaultInferState)

resolve :: Contextual k => String -> Infer (Scheme k)
resolve n = Map.lookup n <$> view environment >>= \case
    Just t  -> return t
    Nothing -> throwError (InferError $ "Type variable" <+> backticks (text n) <+> "is not defined")

lookupTypeName :: String -> Infer (Scheme Type)
lookupTypeName n = Map.lookup n <$> view typeNames >>= \case
    Just t  -> return t
    Nothing -> throwError (InferError $ "Type" <+> backticks (text n) <+> "is not defined")

introduce :: forall k a. Contextual k => String -> Scheme k -> Infer a -> Infer a
introduce n k = local (environment @k %~ Map.insert n k)

freshParam :: Infer String
freshParam = do
    i <- use paramSupply
    let n = "$" ++ show i
    paramSupply += 1
    pure n

remove :: forall proxy k a. Contextual k => proxy k -> String -> Infer a -> Infer a
remove _ n = local (environment @k %~ Map.delete n)

free :: forall proxy k a. Contextual k => proxy k -> String -> Infer a -> Infer a
free _ n inf = do
    t  <- fresh @k n
    t' <- generalize t
    introduce @k n t' $ remove (Of @k) n inf

fresh :: forall k. Inferable k => String -> Infer k
fresh prefix = do
    i <- use (varSupply (Of @k))
    varSupply (Of @k) += 1
    return . varConstr $ prefix ++ show i

generalize :: forall k. Contextual k => k -> Infer (Scheme k)
generalize t = do
    env <- view (environment @k)
    return (Scheme (freeVars t \\ freeVars env) t)

instantiate :: forall k. Kind k => Scheme k -> Infer k
instantiate (Scheme (FreeVars sn tn) t) = do
    svs <- forM (Set.toList sn) $ \ n -> (n,) <$> fresh @Session n
    tvs <- forM (Set.toList tn) $ \ n -> (n,) <$> fresh @Type n
    let s = Subst (Map.fromList svs) (Map.fromList tvs) 
    pure (substitute s t)

bindVar :: forall k. (Eq k, PrettyShow k, Contextual k) => String -> k -> Infer Subst
bindVar u t
    | t == varConstr u     = pure mempty
    | u `Set.member` fkv t = throwError (InferError $ "Occur check fails:" <+> text u <+> "~" <+> prettyShow t)
    | otherwise            = pure (mempty & substEnv .~ Map.singleton u t)
    where fkv = view (freeVarsSet (Of @k))  . freeVars

substituteGlobal :: Subst -> Infer a -> Infer a
substituteGlobal s = local ((typeEnv %~ substitute s) . (sessionEnv %~ substitute s))

instance Kind Type where
    freeVars = \case
        VarT n    -> singletonTV n
        RecordT r -> freeVars r
        QuoteT s  -> freeVars s
        _         -> mempty

    substitute (Subst _ st) (VarT n) = case Map.lookup n st of
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
        VarRT n    -> singletonTV n
        EmptyRT    -> mempty
        ProdRT f r -> freeVars f <> freeVars r
        SumRT f r  -> freeVars f <> freeVars r

    substitute s@(Subst _ st) = \case
        VarRT n -> case Map.lookup n st of
            Just (RecordT rt) -> rt
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
        (SumRT{}, SumRT{}) -> unifyConstr SumRT
        (ProdRT{}, ProdRT{}) -> unifyConstr ProdRT
      where unifyFields x y = foldr (\ (t, t') ms -> ms >>= \ s -> unifyWithSubst s t t') (pure mempty) (Map.intersectionWith (,) x y)
            ((fs, ext), (fs', ext')) = ((,) `on` mapFromRecordType) r r'
            zipMap k x y     = Just (x, y)
            (inner, outer)   = (Map.restrictKeys fs (Map.keysSet fs'), Map.restrictKeys fs ((Set.difference `on` Map.keysSet) fs fs'))
            (inner', outer') = (Map.restrictKeys fs' (Map.keysSet fs), Map.restrictKeys fs' ((Set.difference `on` Map.keysSet) fs' fs))
            err = throwError (InferError $ "Cannot unify record types" <+> backticks (prettyShow r) <+> "and" <+> backticks (prettyShow r'))
            unifyConstr constr = case (ext, ext') of
                (Nothing, Nothing)
                    | ((==) `on` Map.keysSet) fs fs' -> unifyFields inner inner'
                    | otherwise -> err
                (Just extVar, Nothing) -> do
                    s1 <- unifyFields fs inner' <|> err
                    fresh @RecordType "r" >>= \case
                        VarRT nExt' -> do
                            s2 <- bindVar extVar (RecordT $ mapToRecordType constr outer' (Just nExt'))
                            pure (s2 <> s1)
                (Nothing, Just extVar') -> do
                    s1 <- unifyFields inner fs' <|> err
                    fresh @RecordType "r" >>= \case
                        VarRT nExt -> do
                            s2 <- bindVar extVar' (RecordT $ mapToRecordType constr outer (Just nExt))
                            pure (s2 <> s1)
                (Just extVar, Just extVar') -> do
                    s1 <- unifyFields inner fs' <|> err
                    s2 <- (unifyFields `on` substitute s1) fs inner' <|> err
                    (,) <$> fresh @RecordType "r" <*> fresh @RecordType "r" >>= \case
                        (VarRT nExt, VarRT nExt') -> do
                            s3 <- bindVar extVar (substitute (s2 <> s1) (RecordT $ mapToRecordType constr outer' (Just nExt')))
                            s4 <- bindVar extVar' (substitute (s3 <> s2 <> s1) (RecordT $ mapToRecordType constr outer (Just nExt)))
                            pure (s4 <> s3 <> s2 <> s1)

    infer (SumR l x) = do
        (s, t) <- infer x
        ext <- fresh @RecordType "r"
        pure (s, SumRT (Field l t) ext)
    infer (ProdR fs) = do
        rts <- getCompose <$> traverse (infer @Type) (Compose fs)
        ext <- fresh @RecordType "r"
        foldr go (pure (mempty, ext)) rts
      where go (l, (s1, t)) inf = do
                (s2, r) <- inf
                pure (s2 <> s1, ProdRT (Field l t) r)

-- unifyFields :: Fields -> Fields -> Infer Subst
-- unifyFields f1@(MkFields fs ext) f2@(MkFields fs' ext') = case (ext, ext') of
--     (Nothing, Nothing)
--         | ((==) `on` Map.keysSet) fs fs' -> mconcat <*> sequence (Map.mergeWithKey mergeF fs fs')
--         | otherwise -> err
--     (Just extVar, _) -> do
--         s1 <- unifyFields (MkFields fs Nothing) (MkFields inner Nothing) <|> err
--         s2 <- bindVar extVar (MkFields outer ext')
--         return (s2 <> s1)
--   where mergeF f t t' = Just (unify t1 t2)
--         (inner, outer) = (Map.restrictKeys fs' (Map.keysSet fs), Map.restrictKeys fs' ((Set.difference `on` Map.keysSet) fs' fs))
--         err = throwError (InferError $ "Cannot unify fields" <+> backticks (prettyShow f1) <+> "and" <+> backticks (prettyShow f2))

instance Inferable Type where
    type Expr Type = Name

    freeVarsSet _ = freeTypeVars
    varSupply _   = typeVarSupply
    varConstr     = VarT

    unify (VarT u) t = bindVar u t
    unify t (VarT u) = bindVar u t
    unify (RecordT r) (RecordT r') = traceShow (r, r') $ unify r r'
    unify (QuoteT s) (QuoteT s') = unify s s'
    unify t t'
        | t == t'   = pure mempty 
        | otherwise = throwError (InferError $ "Types" <+> backticks (prettyShow t) <+> "and" <+> backticks (prettyShow t') <+> "do not unify")

    -- unify EmptyT (RecordT EmptyT) = pure mempty
    -- unify EmptyT (VariantT EmptyT) = pure mempty
    -- unify (VariantT (ExtendT l t EmptyT)) (RecordT (ExtendT l' t' EmptyT))
    --     | l == l' = unify t t'
    -- unify (RecordT (ExtendT l t EmptyT)) (VariantT (ExtendT l' t' EmptyT))
    --     | l == l' = unify t t'
    -- unify (RecordT EmptyT) (VariantT EmptyT) = pure mempty
    -- unify EmptyT EmptyT = pure mempty
    -- unify (ExtendT label1 fieldTy1 rowTail1) row2@ExtendT{} = do
    --     (fieldTy2, rowTail2, theta1@(Subst th1 _)) <- rewriteRow row2 label1
    --     -- ^ apply side-condition to ensure termination
    --     case snd $ recordToList rowTail1 of
    --         Just tv | Map.member tv th1 -> throwError (InferError "Recursive row type")
    --         _ -> do
    --             theta2 <- unify (substitute theta1 fieldTy1) (substitute theta1 fieldTy2)
    --             let s = theta2 <> theta1
    --             theta3 <- unify (substitute s rowTail1) (substitute s rowTail2)
    --             pure $ theta3 <> s

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
        let s = s2 <> s1
            t = substitute s ft
        pure (s, t)
            -- Nothing -> throwError (InferError $ "Unable to get field" <+> backticks (text f) <+> "of record" <+> backticks (prettyShow r))

instance Contextual Type where
    environment = typeEnv
    substEnv    = typeSubst

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
        s2 <- (unify `on` substitute s1) (substituteNameInSession x (VarN x') s) s'
        pure (s1 <> s2)
    unify NilS NilS = pure mempty
    unify s s' = do
        (ps, ps') <- (,) <$> normalizeParS s <*> normalizeParS s'
        mconcat <$> foldr (<|>)
            (throwError (InferError $ "Sessions" <+> backticks (prettyShow s) <+> "and" <+> backticks (prettyShow s') <+> "do not unify") )
            [ zipWithM unify p p' | (p, p') <- (,) <$> permutations ps <*> permutations ps' ]

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
    infer (NewP n t r) = do
        (s, k) <- introduce n (Scheme mempty t) (infer r)
        pure (s, privatize (VarN n) (substitute s k))
    infer (NewPI n r) = do
        t <- fresh @Type "a"
        (s, k) <- introduce n (Scheme mempty t) (infer r)
        pure (s, privatize (VarN n) (substitute s k))
    infer (ProcP x t p) = do
        t' <-  fresh @Type "a"
        (s1, k) <- introduce x (Scheme mempty t') (infer p)
        s2 <- unifyWithSubst s1 t t'
        let s = s2 <> s1
        pure (s, substitute s (ProcS x t k))
    infer (ProcPI x p) = do
        t <- fresh @Type "a"
        (s, k) <- introduce x (Scheme mempty t) (infer p)
        pure (s, ProcS x (substitute s t) (substitute s k))
    infer (CallP p a) = do
        (s1, k) <- infer p
        (s2, t) <- infer a
        k' <- fresh @Session "s"
        n <- freshParam
        s3 <- introduce n (Scheme mempty t) $ unifyWithSubst (s1 <> s2) k (ProcS n t k')
        let s = s3 <> s2 <> s1
        traceShow s $ pure ()
        pure (s, substituteNameInSession n a (substitute s k'))
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
    infer (VarP n) = do
        sigma <- resolve n
        s <- instantiate sigma
        pure (mempty, s)
    infer NilP = pure (mempty, NilS)

instance Contextual Session where
    environment = sessionEnv
    substEnv    = sessionSubst

-- rewriteRow :: Type -> Label -> Infer (Type, Type, Subst)
-- rewriteRow EmptyT newLabel = throwError (InferError $ "Label" <+> backticks (text newLabel) <+> "cannot be inserted")
-- rewriteRow (ExtendT label fieldTy rowTail) newLabel
--     | newLabel == label     = return (fieldTy, rowTail, mempty)
--     | VarT alpha <- rowTail = do
--         beta  <- fresh "r"
--         gamma <- fresh "a"
--         return ( gamma
--                 , ExtendT label fieldTy beta
--                 , Subst mempty (Map.singleton alpha $ ExtendT newLabel gamma beta)
--                 )
--     | otherwise   = do
--         (fieldTy', rowTail', s) <- rewriteRow rowTail newLabel
--         return ( fieldTy'
--                 , ExtendT label fieldTy rowTail'
--                 , s
--                 )
-- rewriteRow ty _ = throwError (InferError $ "Unexpected type:" <+> backticks (prettyShow ty))

normalizeParS :: Session -> Infer [Session]
normalizeParS (ParS s s') = (liftA2 (++) `on` normalizeParS) s s'
normalizeParS NilS = pure []
normalizeParS s = pure [s]

nameEq :: Name -> Name -> Bool
nameEq QuoteN{} _ = False
nameEq _ QuoteN{} = False
nameEq x y = x == y

-- lookupRecordFieldType :: Label -> Type -> Infer Type
-- lookupRecordFieldType l (IdentT n) = lookupRecordFieldType l =<< instantiate =<< lookupTypeName n
-- lookupRecordFieldType l (VariantT v) =
--     let (fields, ext)  = recordToList v
--     in case Map.lookup l (Map.fromList fields) of
--         Just t  -> pure t
--         Nothing -> throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of variant" <+> backticks (prettyShow (VariantT v)))
-- lookupRecordFieldType l (RecordT r) =
--     let (fields, ext)  = recordToList r
--     in case Map.lookup l (Map.fromList fields) of
--         Just t  -> pure t
--         Nothing -> throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of record" <+> backticks (prettyShow (RecordT r)))
-- lookupRecordFieldType l t = throwError (InferError $ "Cannot get field" <+> backticks (text l) <+> "of non-record type" <+> backticks (prettyShow t))

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

inferProcess' :: Process -> Infer (Scheme Session)
inferProcess' p = do
    (s, k) <- infer p
    generalize (substitute s k)
