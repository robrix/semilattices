{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Meet where

import Data.Lower
import Data.Semigroup
import Data.Set
import Data.Upper

-- $setup
-- >>> import Test.QuickCheck

-- | A meet semilattice is an idempotent commutative semigroup.
class Meet s where
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
  --   Additionally, if @s@ has an 'Upper' bound, then 'upper' must be its left- and right-identity:
  --
  --   > upper /\ a = a
  --   > a /\ upper = a
  --
  --   If @s@ has a 'Lower' bound, then 'lower' must be its left- and right-annihilator:
  --
  --   > lower /\ a = lower
  --   > a /\ lower = lower
  (/\) :: s -> s -> s

  infixr 7 /\


instance Meet () where
  _ /\ _ = ()

-- | Boolean conjunction forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: Bool)
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: Bool)
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: Bool)
--
--   Identity:
--   prop> \ a -> upper /\ a == (a :: Bool)
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: Bool)
instance Meet Bool where
  (/\) = (&&)

-- | Orderings form a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: Ordering)
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: Ordering)
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: Ordering)
--
--   Identity:
--   prop> \ a -> upper /\ a == (a :: Ordering)
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: Ordering)
instance Meet Ordering where
  LT /\ _ = LT
  _ /\ LT = LT
  GT /\ b = b
  a /\ GT = a
  _ /\ _ = EQ

instance Ord a => Meet (Min a) where
  (/\) = (<>)

-- | Set intersection forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: Set Char)
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: Set Char)
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: Set Char)
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: Set Char)
instance Ord a => Meet (Set a) where
  (/\) = intersection


newtype Meeting a = Meeting { getMeeting :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Meet, Num, Ord, Read, Show, Traversable, Upper)

instance Meet a => Semigroup (Meeting a) where
  (<>) = (/\)

instance (Upper a, Meet a) => Monoid (Meeting a) where
  mappend = (<>)
  mempty = upper


-- | Orderings form a meet semilattice.
--
--   Idempotence:
--   prop> \ x -> Met x /\ Met x == Met (x :: Int)
--
--   Associativity:
--   prop> \ a b c -> Met a /\ (Met b /\ Met c) == (Met a /\ Met b) /\ (Met (c :: Int))
--
--   Commutativity:
--   prop> \ a b -> Met a /\ Met b == Met b /\ Met (a :: Int)
--
--   Identity:
--   prop> \ a -> upper /\ Met a == Met (a :: Int)
--
--   Absorption:
--   prop> \ a -> lower /\ Met a == (lower :: Met Int)
newtype Met a = Met { getMet :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)

instance Ord a => Meet (Met a) where
  a /\ b
    | a <= b    = a
    | otherwise = b


newtype GreaterThan a = GreaterThan { getGreaterThan :: a }
  deriving (Enum, Eq, Foldable, Functor, Meet, Num, Read, Show, Traversable)

instance (Eq a, Meet a) => Ord (GreaterThan a) where
  compare a b
    | a == b      = EQ
    | a /\ b == a = LT
    | otherwise   = GT

  a <= b = a /\ b == a
