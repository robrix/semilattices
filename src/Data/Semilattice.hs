module Data.Semilattice where

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
  (\/) :: s -> s -> s

class MeetSemilattice s where
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
