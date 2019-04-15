{-# LANGUAGE
    TemplateHaskell
  , GeneralizedNewtypeDeriving
  , DeriveFoldable
  , DeriveTraversable
  , TypeFamilies
  #-}

module Language.Lys.Types.Env where

import Data.List (intercalate)
import qualified Data.Map as Map

import Data.Align
import Data.Align.Key
import Data.Key hiding (zip)

import Control.Lens

newtype Env a = Env { _asMap :: Map.Map String a }
    deriving ( Eq, Show
             , Semigroup, Monoid
             , Semialign, Align, Keyed, AlignWithKey
             , Functor, Foldable, Traversable)

type instance Key Env = String
type instance Index (Env a) = String
type instance IxValue (Env a) = a

makeLenses ''Env

instance Ixed (Env a) where
    ix k = asMap.ix k

instance At (Env a) where
    at k = asMap.at k

introduce :: String -> a -> Env a -> Env a
introduce x t (Env m) = Env (Map.insert x t m)

remove :: String -> Env a -> Env a
remove x (Env m) = Env (Map.delete x m)

lookupEnv :: String -> Env a -> Maybe a
lookupEnv x (Env m) = Map.lookup x m 
