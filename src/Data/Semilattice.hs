module Data.Semilattice where

-- | A (bounded) join semilattice is a idempotent commutative monoid.
class JoinSemilattice s where
  bottom :: s

  (\/) :: s -> s -> s
