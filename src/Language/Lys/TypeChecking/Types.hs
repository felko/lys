{-# LANGUAGE
    LambdaCase
  , FlexibleInstances
  , FlexibleContexts
  , ConstrainedClassMethods
  , TypeFamilies
  , GeneralizedNewtypeDeriving
  , BlockArguments
  , OverloadedStrings
  , GADTs
  , Rank2Types
  , UndecidableInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  #-}

module Language.Lys.TypeChecking.Types where

import Language.Lys.Types

import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor.Classes

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data KindProxy :: * -> * where
    Type'    :: KindProxy Type
    Name'    :: KindProxy Name
    Process' :: KindProxy Process

-- | A type that can contain free names that can be substituted
class Contextual a c where
    freeNames  :: proxy c -> a -> Set.Set String
    substitute :: Subst c -> a -> a

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
    deriving Show

instance Contextual a a => Semigroup (Subst a) where
    Subst s <> Subst s' = Subst (Map.map (substitute (Subst s)) s' <> s)

instance Contextual a a => Monoid (Subst a) where
    mempty = Subst mempty

restrict :: String -> Subst a -> Subst a
restrict n (Subst s) = Subst (Map.delete n s)

restrictMany :: Set.Set String -> Subst a -> Subst a
restrictMany ns (Subst s) = Subst (Map.withoutKeys s ns)

(\\) :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
(\\) = Set.difference

singleton :: Ord a => a -> Set.Set a
singleton = Set.singleton

-- | The type that stores the intermediate type bindings for inference
data Context = Context
    { _delta :: Map.Map String Type  -- ^ The global, unrestricted environment
    , _theta :: Map.Map String Type  -- ^ The environment under the linear constraints
    } deriving Show
makeLenses ''Context

instance Semigroup Context where
    Context delta1 theta1 <> Context delta2 theta2 = Context (delta1 <> delta2) (theta1 <> theta2)

instance Monoid Context where
    mempty = Context mempty mempty

-- | Alias for `Map.insert`
introduce :: Ord k => k -> v -> Map.Map k v -> Map.Map k v
introduce = Map.insert

-- | Alias for `Map.delete`
remove :: Ord k => k -> Map.Map k v -> Map.Map k v
remove = Map.delete

data Scheme = Scheme
    { _schemeTypeVars :: [String]
    , _schemeNames    :: [String]
    , _schemeCtx      :: Context }
    deriving Show
makeLenses ''Scheme

instance Contextual Scheme Type where
    freeNames p (Scheme tv _ ctx) = freeNames p ctx \\ Set.fromList tv
    substitute s (Scheme tv nv ctx) = Scheme tv nv (substitute (restrictMany (Set.fromList tv) s) ctx)

instance Contextual Scheme Name where
    freeNames p (Scheme tv nv ctx) = freeNames p ctx \\ Set.fromList nv
    substitute s (Scheme tv nv ctx) = Scheme tv nv (substitute (restrictMany (Set.fromList nv) s) ctx)

instance Contextual Context Type where
    freeNames p (Context d t) = foldMap (freeNames p) d <> foldMap (freeNames p) t
    substitute s = (delta %~ fmap (substitute s))
                 . (theta %~ fmap (substitute s))

instance Contextual Context Name where
    freeNames p (Context d t) = Map.keysSet d <> Map.keysSet t
    substitute (Subst s) (Context d t) = Context (substKeys d) (substKeys t)
        where substKeys e = Map.fromList $ catMaybes
                [ case Map.lookup x s of
                    Just (LitN l) -> Nothing
                    Just (VarN y) -> Just (y, t)
                    Nothing       -> Just (x, t)
                | (x, t) <- Map.toList e ]

instance (Ord k, Contextual v c) => Contextual (Map.Map k v) c where
    freeNames p = foldMap (freeNames p)
    substitute s = fmap (substitute s)

instance Contextual (Map.Map String v) String where
    freeNames _ = Map.keysSet
    substitute (Subst s) m = Map.fromList
        [ case Map.lookup x s of
            Just y  -> (y, t)
            Nothing -> (x, t)
        | (x, t) <- Map.toList m ]

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
        PlusT a b -> freeNames p a <> freeNames p b
        WithT a b -> freeNames p a <> freeNames p b
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
        PlusT a b -> PlusT (substitute s a) (substitute s b)
        WithT a b -> WithT (substitute s a) (substitute s b)
        VarT n -> fromMaybe (VarT n) (Map.lookup n m)
        PrimT t -> PrimT t

newtype InferError = InferError Doc
    deriving Show

instance Semigroup InferError where
    (<>) = flip const

instance Monoid InferError where
    mempty = InferError "Unknown error"

data InferState = InferState
    { _currentSubst  :: Subst Type
    , _typeVarSupply :: Int
    , _nameSupply    :: Int }
    deriving Show
makeLenses ''InferState

defaultInferState :: InferState
defaultInferState = InferState
    { _currentSubst = Subst mempty
    , _typeVarSupply = 0
    , _nameSupply = 0 }

type Gamma = Map.Map String Scheme

newtype Infer a = Infer
    { unInfer :: ReaderT Gamma (StateT InferState (Except InferError)) a }
    deriving ( Functor, Applicative, Alternative, Monad
             , MonadReader Gamma
             , MonadState InferState
             , MonadError InferError )

runInfer :: Infer a -> Gamma -> Either InferError a
runInfer (Infer m) gamma = runExcept (evalStateT (runReaderT m gamma) defaultInferState)


lookupTheta, lookupDelta, lookupDeltaBottom, lookupDeltaOne :: String -> Context -> Infer Type
lookupTheta x ctx = case Map.lookup x (ctx ^. theta) of
    Just t -> pure t
    Nothing -> throwError (InferError $ "Name" <+> string x <+> "is not in scope")

lookupDelta x ctx = case Map.lookup x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> throwError (InferError $ "Name" <+> string x <+> "is not in scope")

lookupDeltaBottom x ctx = case Map.lookup x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> pure BottomT

lookupDeltaOne x ctx = case Map.lookup x (ctx ^. delta) of
    Just t -> pure t
    Nothing -> pure OneT

lookupGamma :: String -> Infer Scheme
lookupGamma x = asks (Map.lookup x) >>= \case
    Just ctx -> pure ctx
    Nothing  -> throwError (InferError $ "Process" <+> string x <+> "is not defined")

instantiate :: Scheme -> Infer Context
instantiate (Scheme tv nv ctx) = do
    ts <- mapM freshType tv
    ns <- mapM freshName nv
    let st = Subst (Map.fromList (zip tv ts))
        sn = Subst (Map.fromList (zip nv ns))
    pure (substitute st . substitute sn $ ctx)

freshType :: String -> Infer Type
freshType prefix = do
    i <- use typeVarSupply
    typeVarSupply += 1
    pure (VarT (prefix ++ show i))

freshName :: String -> Infer Name
freshName prefix = do
    i <- use nameSupply
    nameSupply += 1
    pure (VarN (prefix ++ show i))

subst :: Subst Type -> Infer ()
subst = modifying currentSubst . (<>)

class Contextual a c => Unifiable a c where
    unify :: a -> a -> Infer (Subst c)

instance Unifiable Context Name where
    unify = undefined

infix 0 :⊢

data Judgement a = (:⊢)
    { _judged   :: a
    , _judgeCtx :: Context }
makeLenses ''Judgement

class Inferable a where
    type TypeOf a :: *
    infer :: Judgement a -> Infer Context
