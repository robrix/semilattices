{-# LANGUAGE DefaultSignatures, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Lower where

import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Set
import Data.Type.Equality

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
instance Lower Char
instance Lower Int
instance (Lower a, Lower b) => Lower (a, b) where bottom = (bottom, bottom)
instance (Lower a, Lower b, Lower c) => Lower (a, b, c) where bottom = (bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d) => Lower (a, b, c, d) where bottom = (bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e) => Lower (a, b, c, d, e) where bottom = (bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f) => Lower (a, b, c, d, e, f) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g) => Lower (a, b, c, d, e, f, g) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h) => Lower (a, b, c, d, e, f, g, h) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i) => Lower (a, b, c, d, e, f, g, h, i) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j) => Lower (a, b, c, d, e, f, g, h, i, j) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k) => Lower (a, b, c, d, e, f, g, h, i, j, k) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l) => Lower (a, b, c, d, e, f, g, h, i, j, k, l) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)
instance (Lower a, Lower b, Lower c, Lower d, Lower e, Lower f, Lower g, Lower h, Lower i, Lower j, Lower k, Lower l, Lower m, Lower n, Lower o) => Lower (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where bottom = (bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom, bottom)


-- Data.Proxy
instance Lower (Proxy a)

-- Data.Semigroup
instance Lower a => Lower (Max a) where bottom = Max bottom

-- Data.Type.Equality
instance a ~ b => Lower (a :~: b)

-- containers
instance Lower (Set a) where
  bottom = empty
