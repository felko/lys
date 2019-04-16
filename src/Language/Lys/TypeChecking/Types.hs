{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , ConstrainedClassMethods
  , ConstraintKinds
  , TypeFamilies
  , TypeApplications
  , TupleSections
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  , OverloadedStrings
  , GADTs
  , Rank2Types
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , TemplateHaskell
  #-}

module Language.Lys.TypeChecking.Types where

import Language.Lys.Core
import Language.Lys.Pretty
import Language.Lys.Types.Context
import Language.Lys.Types.Env
import Language.Lys.Types

import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.These
import Data.Align
import Data.Align.Key

import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap
import qualified Data.Set as Set
import Data.Functor.Classes

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), align)

import Control.Lens hiding (Context)

data KindProxy :: * -> * where
    Type'    :: KindProxy Type
    Name'    :: KindProxy Name
    Process' :: KindProxy Process

-- | A type that can contain free names that can be substituted
class Contextual a c where
    freeNames  :: proxy c -> a -> Set.Set String
    substitute :: Subst c -> a -> a

type Substituable a = Contextual a a

substitute1 :: Contextual a c => String -> c -> a -> a
substitute1 x y = substitute (Subst (Map.singleton x y))

-- | Type supporting alpha equivalence (equality up to renaming)
class Eq a => AlphaEq a where
    (≡∝) :: a -> a -> Bool

-- | Type supporting structural equivalence
class AlphaEq a => StructEq a where
    (≡) :: a -> a -> Bool

-- | A type whose values can have a dual
class Dual a where
    -- | Gives the dual object. The implementation should verify `dual . dual = id`
    dual :: a -> a

newtype Subst a = Subst
    { fromSubst :: Map.Map String a }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Subst a) where
    pretty (Subst m)
        | Map.null m = "•"
        | otherwise  = cat $ punctuate ", " (map (\ (x, t) -> string x <> " -> " <> pretty t) (Map.assocs m))

instance Contextual a a => Semigroup (Subst a) where
    Subst s <> Subst s' = Subst (Map.map (substitute (Subst s)) s' <> s)

instance Contextual a a => Monoid (Subst a) where
    mempty = Subst mempty

instance Ord k => Semigroup (MMap.MultiMap k v) where
    (<>) = MMap.foldrWithKey \ k x acc -> MMap.insert k x acc

instance Ord k => Monoid (MMap.MultiMap k v) where
    mempty = MMap.empty

restrict :: String -> Subst a -> Subst a
restrict n (Subst s) = Subst (Map.delete n s)

restrictMany :: Set.Set String -> Subst a -> Subst a
restrictMany ns (Subst s) = Subst (Map.withoutKeys s ns)

(\\) :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
(\\) = Set.difference

singleton :: Ord a => a -> Set.Set a
singleton = Set.singleton

data Scheme a c = Scheme
    { _schemeVars :: [String]
    , _schemeVal  :: c }
    deriving (Show, Functor)
makeLenses ''Scheme

instance Contextual (Scheme Type Context) Name where
    freeNames p (Scheme vs x) = freeNames p x \\ Set.fromList vs
    substitute s (Scheme vs x) = Scheme vs (substitute (restrictMany (Set.fromList vs) s) x)

instance Contextual (Scheme Name Context) Type where
    freeNames p (Scheme vs x) = freeNames p x \\ Set.fromList vs
    substitute s (Scheme vs x) = Scheme vs (substitute (restrictMany (Set.fromList vs) s) x)

instance Contextual Context Type where
    freeNames p (Context d t) = foldMap (freeNames p) d <> foldMap (freeNames p) t
    substitute s = (delta %~ fmap (substitute s))
                 . (theta %~ fmap (substitute s))

instance Contextual Context Name where
    freeNames p (Context (Env d) (Env t)) = Map.keysSet d <> Map.keysSet t
    substitute (Subst s) (Context d t) = Context (substKeys d) (substKeys t)
        where substKeys (Env e) = Env . Map.fromList $ catMaybes
                [ case Map.lookup x s of
                    Just (LitN l) -> Nothing
                    Just (VarN y) -> Just (y, t)
                    Nothing       -> Just (x, t)
                | (x, t) <- Map.toList e ]

instance (Ord k, Contextual v c) => Contextual (Map.Map k v) c where
    freeNames = foldMap . freeNames
    substitute = fmap . substitute

instance Contextual a c => Contextual (Env a) c where
    freeNames d e = foldMap (freeNames d) e
    substitute = fmap . substitute

instance Contextual Name Name where
    freeNames _ (VarN x) = singleton x
    freeNames _ LitN{} = mempty
    substitute (Subst s) (VarN x) = fromMaybe (VarN x) (Map.lookup x s)
    substitute _ (LitN l) = LitN l

instance Contextual Type Type where
    freeNames p = \case
        TopT -> mempty
        BottomT -> mempty
        OneT -> mempty
        ZeroT -> mempty
        OfCourseT t -> freeNames p t
        WhyNotT t -> freeNames p t
        TensorT a b -> freeNames p a <> freeNames p b
        ParT a b -> freeNames p a <> freeNames p b
        PlusT fs -> foldMap (freeNames p) fs
        WithT fs -> foldMap (freeNames p) fs
        VarT n -> singleton n
        PrimT{} -> mempty

    substitute s@(Subst m) = \case
        TopT -> TopT
        BottomT -> BottomT
        OneT -> OneT
        ZeroT -> ZeroT
        OfCourseT t -> OfCourseT (substitute s t)
        WhyNotT t -> WhyNotT (substitute s t)
        TensorT a b -> TensorT (substitute s a) (substitute s b)
        ParT a b -> ParT (substitute s a) (substitute s b)
        PlusT fs -> PlusT (substitute s <$> fs)
        WithT fs -> WithT (substitute s <$> fs)
        VarT n -> fromMaybe (VarT n) (Map.lookup n m)
        IdentT n -> IdentT n
        DualT t -> DualT (substitute (dual <$> s) t)
        AppT t ts -> AppT (substitute s t) (substitute s <$> ts)
        PrimT t -> PrimT t

instance Dual Type where
    dual = \case
        DualT (DualT a) -> dual a
        DualT a -> a
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
        AppT t ts -> AppT (DualT t) ts
        IdentT n -> DualT (IdentT n)

newtype TypeError = TypeError Doc
    deriving Show

instance Pretty TypeError where
    pretty (TypeError t) = t

type TypeErrors = [TypeError]

data InferState = InferState
    { _typeVarSupply :: Int
    , _depth         :: Int }
    deriving Show
makeLenses ''InferState

defaultInferState :: InferState
defaultInferState = InferState
    { _typeVarSupply = 0
    , _depth         = 0 }

type Gamma = Env (Scheme Type (Scheme Name Context))
type Tau = Env (Scheme Type Type)

data GlobalEnv = GlobalEnv
    { _gamma :: Gamma
    , _tau   :: Tau }
    deriving (Show)
makeLenses ''GlobalEnv

instance Semigroup GlobalEnv where
    GlobalEnv g t <> GlobalEnv g' t' = GlobalEnv (g <> g') (t <> t')

instance Monoid GlobalEnv where
    mempty = GlobalEnv mempty mempty

newtype Infer a = Infer
    { unInfer :: ReaderT GlobalEnv (StateT InferState (Except TypeErrors)) a }
    deriving ( Functor, Applicative, Alternative, Monad
             , MonadReader GlobalEnv
             , MonadState InferState
             , MonadError TypeErrors )

throwError1 :: MonadError [e] m => e -> m a
throwError1 = throwError . pure

runInfer :: Infer a -> GlobalEnv -> Either TypeErrors a
runInfer (Infer m) env = runExcept (evalStateT (runReaderT m env) defaultInferState)


lookupTheta, lookupDelta, lookupDeltaBottom, lookupDeltaOne :: String -> Context -> Infer Type
lookupTheta x ctx = case lookupEnv x (ctx ^. theta) of
    Just t -> pure t
    Nothing -> throwError1 (TypeError $ "Unrestricted name" <+> string x <+> "is not in scope")

lookupDelta x ctx = case lookupEnv x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> throwError1 (TypeError $ "Name" <+> string x <+> "is not in scope")

lookupDeltaBottom x ctx = case lookupEnv x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> pure BottomT

lookupDeltaOne x ctx = case lookupEnv x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> pure OneT

lookupGamma :: String -> Infer (Scheme Type (Scheme Name Context))
lookupGamma p = asks (view $ gamma.at p) >>= \case
    Just ctx -> pure ctx
    Nothing  -> throwError1 (TypeError $ "Process" <+> string p <+> "is not defined")

lookupTau :: String -> Infer (Scheme Type Type)
lookupTau n = asks (view $ tau.at n) >>= \case
    Just t -> pure t
    Nothing  -> throwError1 (TypeError $ "Type" <+> string n <+> "is not defined")

call :: Contextual c Name => [Name] -> Scheme Name c -> Infer c
call ns (Scheme nv t) = do
    let s = Subst (Map.fromList (zip nv ns))
    pure (substitute s t)

instantiate :: Contextual c Type => Scheme Type c -> Infer c
instantiate (Scheme tv t) = do
    ts <- mapM freshType tv
    let s = Subst (Map.fromList (zip tv ts))
    pure (substitute s t)

instantiateCall :: [Name] -> Scheme Type (Scheme Name Context) -> Infer Context
instantiateCall ns s = instantiate s >>= call ns

freshType :: String -> Infer Type
freshType prefix = do
    i <- use typeVarSupply
    typeVarSupply += 1
    pure (VarT ('$':prefix ++ show i))

class Contextual a c => Unifiable a c where
    unify :: a -> a -> Infer (Subst c)

class Unifiable a c => Unified a c where
    unified :: a -> a -> Infer (a, Subst c)

unified' :: forall proxy a c. Unified a c => proxy c -> a -> a -> Infer a
unified' _ x y = fst <$> unified @a @c x y

instance (Show a, Unifiable a c, Substituable c) => Unifiable (Env a) c where
    unify e e' = fold <$> sequence (alignWithKey f e e')
        where f k = these (const err) (const err) unify
              err = throwError1 (TypeError $ "Couldn't unify environments" <+> string (show e) <+> "and" <+> string (show e'))

unzipEnv :: Env (a, b) -> (Env a, Env b)
unzipEnv e = (fst <$> e, snd <$> e)

instance (Show a, Unified a c, Substituable c) => Unified (Env a) c where
    unified e e' = (_2 %~ fold) . unzipEnv <$> sequence (alignWithKey f e e')
        where f _ = these (pure . (,mempty @(Subst c))) (pure . (,mempty)) unified

complete :: Context -> Infer ()
complete (Context (Env d) _)
    | Map.null d = pure ()
    | otherwise  = throwError1 (TypeError "Environment is not complete")

infix 0 :⊢

data Judgement a = (:⊢)
    { _judged   :: a
    , _judgeCtx :: Context }
makeLenses ''Judgement

class Inferable a where
    type TypeOf a :: *
    infer :: Judgement a -> Infer (Context, Subst (TypeOf a))

data CheckError
    = TypeCheckError TypeError
    deriving Show

instance Pretty CheckError where
    pretty = \case
        TypeCheckError e -> pretty e

type CheckErrors = [CheckError]

newtype Check a = Check
    { unCheck :: StateT GlobalEnv (Except CheckErrors) a }
    deriving ( Functor, Applicative, Alternative, Monad
             , MonadState GlobalEnv
             , MonadError CheckErrors )

runCheck :: Check a -> GlobalEnv -> Either CheckErrors (a, GlobalEnv)
runCheck (Check m) e = runExcept (runStateT m e)

evalCheck :: Check a -> GlobalEnv -> Either CheckErrors a
evalCheck c e = fst <$> runCheck c e

class Checkable a c | a -> c where
    check :: a -> Check c

checkInfer :: Infer a -> Check a
checkInfer m = runInfer m <$> get >>= \case
    Left errs -> throwError (TypeCheckError <$> errs)
    Right val -> pure val
