{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | Total 'Ord'erings give rise to 'Join' and 'Meet' semilattices.
module Data.Semilattice.Order
( Order(..)
) where

import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper

-- | A 'Join'- and 'Meet'-semilattice for any total 'Ord'ering.
newtype Order a = Order { getOrder :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)

-- | Total 'Ord'erings give rise to a join semilattice satisfying:
--
--   Idempotence:
--
--   prop> Order x \/ Order x == Order x
--
--   Associativity:
--
--   prop> Order a \/ (Order b \/ Order c) == (Order a \/ Order b) \/ Order c
--
--   Commutativity:
--
--   prop> Order a \/ Order b == Order b \/ Order a
--
--   Identity:
--
--   prop> lowerBound \/ Order a == Order (a :: Int)
--
--   Absorption:
--
--   prop> upperBound \/ Order a == (upperBound :: Order Int)
--
--   Distributivity:
--
--   prop> Order a \/ Order b /\ Order c == (Order a \/ Order b) /\ (Order a \/ Order c)
instance Ord a => Join (Order a) where
  a \/ b
    | a <= b    = b
    | otherwise = a

-- | Total 'Ord'erings give rise to a meet semilattice satisfying:
--
--   Idempotence:
--
--   prop> Order x /\ Order x == Order x
--
--   Associativity:
--
--   prop> Order a /\ (Order b /\ Order c) == (Order a /\ Order b) /\ Order c
--
--   Commutativity:
--
--   prop> Order a /\ Order b == Order b /\ Order a
--
--   Identity:
--
--   prop> upperBound /\ Order a == Order (a :: Int)
--
--   Absorption:
--
--   prop> lowerBound /\ Order a == (lowerBound :: Order Int)
--
--   Distributivity:
--
--   prop> Order a /\ (Order b \/ Order c) == Order a /\ Order b \/ Order a /\ Order c
instance Ord a => Meet (Order a) where
  a /\ b
    | a <= b    = a
    | otherwise = b


-- $setup
-- >>> import Test.QuickCheck
