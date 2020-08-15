{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances
module Language.Pion.Orphans where

import Data.GADT.Compare

instance Category GOrdering where
  id = GEQ
  GEQ . o = o
  o . GEQ = o
  _ . GLT = GLT
  _ . GGT = GGT
