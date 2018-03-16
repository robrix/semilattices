module Data.Join where

import Data.Semigroup
import qualified Data.Set as Set

-- | A join semilattice is an idempotent commutative semigroup.
class Join s where
  -- | The join operation.
  --
  --   Laws:
  --
  --   Idempotence:
  --
  --   > x \/ x = x
  --
  --   Associativity:
  --
  --   > a \/ (b \/ c) = (a \/ b) \/ c
  --
  --   Commutativity:
  --
  --   > a \/ b = b \/ a
  --
  --   Additionally, if @s@ has a 'Lower', the identity law must hold:
  --
  --   > bottom \/ a = a
  (\/) :: s -> s -> s

  infixr 6 \/


instance Join () where
  _ \/ _ = ()

instance Join Bool where
  (\/) = (||)

instance Ord a => Join (Max a) where
  (\/) = (<>)

instance Ord a => Join (Set.Set a) where
  (\/) = Set.union
