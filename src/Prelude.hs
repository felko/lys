-- | Reexportation of relude plus small utility functions
module Prelude
  ( module Relude,
    module Control.Category,
    enumerate,
    some,
    foldl1',
    foldr1,
  )
where

import Control.Category
import Relude hiding
  ( State,
    Type,
    evalState,
    execState,
    group,
    id,
    runState,
    some,
    (.),
  )
import Relude.Extra.Foldable1 (foldl1')

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

some :: Alternative f => f a -> f (NonEmpty a)
some x = (:|) <$> x <*> many x

foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 f (x :| xs) = foldr f x xs
