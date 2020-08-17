{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances
module Language.Pion.Orphans () where

import Data.GADT.Compare (GOrdering (..))
import Data.Some
import Prettyprinter

instance Category GOrdering where
  id = GEQ
  GEQ . o = o
  o . GEQ = o
  _ . GLT = GLT
  _ . GGT = GGT

instance (forall x. Pretty (f x)) => Pretty (Some f) where
  pretty (Some f) = pretty f
