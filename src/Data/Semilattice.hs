module Data.Semilattice where

import qualified Data.Set as Set

-- | A join semilattice is an idempotent commutative semigroup.
class JoinSemilattice s where
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
  --   Additionally, if @s@ has a 'LowerBound', the identity law must hold:
  --
  --   > bottom \/ a = a
  (\/) :: s -> s -> s

class MeetSemilattice s where
  -- | The meet operation.
  --
  --   Laws:
  --
  --   Idempotence:
  --
  --   > x /\ x = x
  --
  --   Associativity:
  --
  --   > a /\ (b /\ c) = (a /\ b) /\ c
  --
  --   Commutativity:
  --
  --   > a /\ b = b /\ a
  --
  --   Additionally, if @s@ has an 'UpperBound', the identity law must hold:
  --
  --   > top /\ a = a
  (/\) :: s -> s -> s

class LowerBound s where
  bottom :: s

class UpperBound s where
  top :: s


instance JoinSemilattice () where
  _ \/ _ = ()

instance MeetSemilattice () where
  _ /\ _ = ()

instance LowerBound () where
  bottom = ()

instance UpperBound () where
  top = ()


instance Ord a => JoinSemilattice (Set.Set a) where
  (\/) = Set.union
