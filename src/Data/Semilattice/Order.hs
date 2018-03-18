{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Order where

import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper

-- $setup
-- >>> import Test.QuickCheck

-- | A 'Join'- and 'Meet'-semilattice for any total 'Ord'ering.
newtype Order a = Order { getOrder :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)

-- | Total 'Ord'erings give rise to a join semilattice satisfying:
--
--   Idempotence:
--   prop> \ x -> Order x \/ Order x == Order x
--
--   Associativity:
--   prop> \ a b c -> Order a \/ (Order b \/ Order c) == (Order a \/ Order b) \/ Order c
--
--   Commutativity:
--   prop> \ a b -> Order a \/ Order b == Order b \/ Order a
--
--   Identity:
--   prop> \ a -> lower \/ Order a == Order (a :: Int)
--
--   Absorption:
--   prop> \ a -> upper \/ Order a == (upper :: Order Int)
--
--   Distributivity:
--   prop> \ a b c -> Order a \/ Order b /\ Order c == (Order a \/ Order b) /\ (Order a \/ Order c)
instance Ord a => Join (Order a) where
  a \/ b
    | a <= b    = b
    | otherwise = a

-- | Total 'Ord'erings give rise to a meet semilattice satisfying:
--
--   Idempotence:
--   prop> \ x -> Order x /\ Order x == Order x
--
--   Associativity:
--   prop> \ a b c -> Order a /\ (Order b /\ Order c) == (Order a /\ Order b) /\ Order c
--
--   Commutativity:
--   prop> \ a b -> Order a /\ Order b == Order b /\ Order a
--
--   Identity:
--   prop> \ a -> upper /\ Order a == Order (a :: Int)
--
--   Absorption:
--   prop> \ a -> lower /\ Order a == (lower :: Order Int)
--
--   Distributivity:
--   prop> \ a b c -> Order a /\ (Order b \/ Order c) == Order a /\ Order b \/ Order a /\ Order c
instance Ord a => Meet (Order a) where
  a /\ b
    | a <= b    = a
    | otherwise = b
