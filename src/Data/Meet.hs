{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Meet where

import Data.Semigroup
import Data.Set
import Data.Upper

-- $setup
-- >>> import Data.Lower
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
  --   Additionally, if @s@ has an 'Upper' bound, then 'top' must be its left- and right-identity:
  --
  --   > top /\ a = a
  --   > a /\ top = a
  --
  --   If @s@ has a 'Lower' bound, then 'bottom' must be its left- and right-annihilator:
  --
  --   > bottom /\ a = bottom
  --   > a /\ bottom = bottom
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
--   prop> \ a -> top /\ a == (a :: Bool)
--
--   Absorption:
--   prop> \ a -> bottom /\ a == (bottom :: Bool)
instance Meet Bool where
  (/\) = (&&)

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
--   prop> \ a -> bottom /\ a == (bottom :: Set Char)
instance Ord a => Meet (Set a) where
  (/\) = intersection


newtype Meeting a = Meeting { getMeeting :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Meet, Num, Ord, Read, Show, Traversable, Upper)

instance Meet a => Semigroup (Meeting a) where
  (<>) = (/\)

instance (Upper a, Meet a) => Monoid (Meeting a) where
  mappend = (<>)
  mempty = top


newtype GreaterThan a = GreaterThan { getGreaterThan :: a }
  deriving (Enum, Eq, Foldable, Functor, Meet, Num, Read, Show, Traversable)

instance (Eq a, Meet a) => Ord (GreaterThan a) where
  compare a b
    | a == b      = EQ
    | a /\ b == a = LT
    | otherwise   = GT

  a <= b = a /\ b == a
