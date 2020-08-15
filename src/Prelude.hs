-- | Reexportation of relude plus small utility functions
module Prelude
  ( module Relude,
    module Control.Category,
    enumerate,
  )
where

import Control.Category
import Relude hiding (Type, id, (.))

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]
