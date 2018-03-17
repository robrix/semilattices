{-# LANGUAGE DefaultSignatures, PolyKinds #-}
module Data.Lower where

import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Set

class Lower s where
  -- | The greatest lower bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'bottom' and 'minBound' to agree:
  --
  --   > bottom = minBound
  --
  --   If @s@ is a 'Join', 'bottom' must be the identity of '(\/)':
  --
  --   > bottom \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'bottom' must be at least as small as every terminating value:
  --
  --   > compare bottom a /= GT
  bottom :: s
  default bottom :: Bounded s => s
  bottom = minBound


-- Prelude
instance Lower ()
instance Lower Bool
instance Lower Ordering
instance Lower Int


-- Data.Proxy
instance Lower (Proxy a)

-- Data.Semigroup
instance Lower a => Lower (Max a) where bottom = Max bottom

instance Lower (Set a) where
  bottom = empty
