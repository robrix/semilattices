module Data.Semilattice where

-- | A join semilattice is an idempotent commutative semigroup.
class JoinSemilattice s where
  (\/) :: s -> s -> s

class LowerBounded s where
  bottom :: s

class UpperBounded s where
  top :: s
