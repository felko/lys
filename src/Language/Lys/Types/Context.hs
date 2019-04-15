{-# LANGUAGE TemplateHaskell #-}

module Language.Lys.Types.Context where

import Language.Lys.Types.Env
import Language.Lys.Types

import Control.Lens hiding (Context)

-- | The type that stores the intermediate type bindings for inference
data Context = Context
    { _delta :: Env Type  -- ^ The global, unrestricted environment
    , _theta :: Env Type  -- ^ The environment under the linear constraints
    } deriving (Eq, Show)
makeLenses ''Context

instance Semigroup Context where
    Context delta1 theta1 <> Context delta2 theta2 = Context (delta1 <> delta2) (theta1 <> theta2)

instance Monoid Context where
    mempty = Context mempty mempty
