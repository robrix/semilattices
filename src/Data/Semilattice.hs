{-# LANGUAGE DefaultSignatures #-}
module Data.Semilattice where

-- | A (bounded) join semilattice is a idempotent commutative monoid.
class JoinSemilattice s where
  bottom :: s
  default bottom :: Monoid s => s
  bottom = mempty

  (\/) :: s -> s -> s
  default (\/) :: Monoid s => s -> s -> s
  (\/) = mappend
