module Data.Lower where

import Data.Semigroup
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


instance Lower () where
  bottom = ()

instance Lower Bool where
  bottom = False

instance Bounded a => Lower (Max a) where
  bottom = minBound

instance Lower (Set a) where
  bottom = empty
