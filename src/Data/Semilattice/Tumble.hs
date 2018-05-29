{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | Inverting a 'Join' semilattice gives rise to a 'Meet' semilattice, and vice versa.
module Data.Semilattice.Tumble where

import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper

-- | 'Tumble' gives a 'Join' semilattice for any 'Meet' semilattice and vice versa, 'Lower' bounds for 'Upper' bounds and vice versa, and swaps the bounds of 'Bounded' instances.
newtype Tumble a = Tumble { getTumble :: a }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

-- $
--
-- Idempotence:
--
-- prop> \ x -> x /\ x == (x :: Tumble Bool)
--
-- Associativity:
--
-- prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: Tumble Bool)
--
-- Commutativity:
--
-- prop> \ a b -> a /\ b == b /\ (a :: Tumble Bool)
--
-- Identity:
--
-- prop> \ a -> upperBound /\ a == (a :: Tumble Bool)
--
-- Absorption:
--
-- prop> \ a -> lowerBound /\ a == (lowerBound :: Tumble Bool)
instance Join a => Meet (Tumble a) where
  Tumble a /\ Tumble b = Tumble (a \/ b)

-- $
--
-- Idempotence:
--
-- prop> \ x -> x \/ x == (x :: Tumble Bool)
--
-- Associativity:
--
-- prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: Tumble Bool)
--
-- Commutativity:
--
-- prop> \ a b -> a \/ b == b \/ (a :: Tumble Bool)
--
-- Identity:
--
-- prop> \ a -> lowerBound \/ a == (a :: Tumble Bool)
--
-- Absorption:
--
-- prop> \ a -> upperBound \/ a == (upperBound :: Tumble Bool)
instance Meet a => Join (Tumble a) where
  Tumble a \/ Tumble b = Tumble (a /\ b)

instance Bounded a => Bounded (Tumble a) where
  minBound = Tumble maxBound
  maxBound = Tumble minBound

-- $
--
-- Bounded:
--
-- prop> upperBound == (maxBound :: Tumble Bool)
--
-- Identity of '/\':
--
-- prop> upperBound /\ a == (a :: Tumble Bool)
--
-- Absorbing element of '\/':
--
-- prop> upperBound \/ a == (upperBound :: Tumble Bool)
--
-- Ord:
--
-- prop> compare upperBound (a :: Tumble Bool) /= LT
instance Lower a => Upper (Tumble a) where
  upperBound = Tumble lowerBound

-- $
--
-- Bounded:
--
-- prop> lowerBound == (minBound :: Tumble Bool)
--
-- Identity of '\/':
--
-- prop> lowerBound \/ a == (a :: Tumble Bool)
--
-- Absorbing element of '/\':
--
-- prop> lowerBound /\ a == (lowerBound :: Tumble Bool)
--
-- Ord:
--
-- prop> compare lowerBound (a :: Tumble Bool) /= GT
instance Upper a => Lower (Tumble a) where
  lowerBound = Tumble upperBound


-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary a => Arbitrary (Tumble a) where arbitrary = Tumble <$> arbitrary ; shrink (Tumble a) = Tumble <$> shrink a
