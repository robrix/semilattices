{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Join where

import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Lower
import Data.Map as Map
import Data.Semigroup
import Data.Set as Set
import Data.Upper

-- $setup
-- >>> import Test.QuickCheck

-- | A join semilattice is an idempotent commutative semigroup.
class Join s where
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
  --   Additionally, if @s@ has a 'Lower' bound, then 'lower' must be its left- and right-identity:
  --
  --   > lower \/ a = a
  --   > a \/ lower = a
  --
  --   If @s@ has an 'Upper' bound, then 'upper' must be its left- and right-annihilator:
  --
  --   > upper \/ a = upper
  --   > a \/ upper = upper
  (\/) :: s -> s -> s

  infixr 6 \/


-- Prelude

instance Join () where
  _ \/ _ = ()

-- | Boolean disjunction forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: Bool)
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: Bool)
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: Bool)
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: Bool)
--
--   Absorption:
--   prop> \ a -> upper \/ a == (upper :: Bool)
instance Join Bool where
  (\/) = (||)

-- | Orderings form a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: Ordering)
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: Ordering)
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: Ordering)
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: Ordering)
--
--   Absorption:
--   prop> \ a -> upper \/ a == (upper :: Ordering)
instance Join Ordering where
  GT \/ _ = GT
  _ \/ GT = GT
  LT \/ b = b
  a \/ LT = a
  _ \/ _ = EQ


-- Data.Semigroup

instance Ord a => Join (Max a) where
  (\/) = (<>)


-- containers

-- | IntMap union with 'Join'able values forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: IntMap (Set Char))
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: IntMap (Set Char))
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: IntMap (Set Char))
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: IntMap (Set Char))
instance Join a => Join (IntMap a) where
  (\/) = IntMap.unionWith (\/)

-- | IntSet union forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: IntSet)
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: IntSet)
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: IntSet)
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: IntSet)
instance Join IntSet where
  (\/) = IntSet.union

-- | Map union with 'Join'able values forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: Map Char (Set Char))
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: Map Char (Set Char))
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: Map Char (Set Char))
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: Map Char (Set Char))
instance (Ord k, Join a) => Join (Map k a) where
  (\/) = Map.unionWith (\/)

-- | Set union forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x \/ x == (x :: Set Char)
--
--   Associativity:
--   prop> \ a b c -> a \/ (b \/ c) == (a \/ b) \/ (c :: Set Char)
--
--   Commutativity:
--   prop> \ a b -> a \/ b == b \/ (a :: Set Char)
--
--   Identity:
--   prop> \ a -> lower \/ a == (a :: Set Char)
instance Ord a => Join (Set a) where
  (\/) = Set.union


newtype Joining a = Joining { getJoining :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Join, Lower, Num, Ord, Read, Show, Traversable)

instance Join a => Semigroup (Joining a) where
  (<>) = (\/)

instance (Lower a, Join a) => Monoid (Joining a) where
  mappend = (<>)
  mempty = lower


-- | Orderings form a join semilattice.
--
--   Idempotence:
--   prop> \ x -> Joined x \/ Joined x == Joined (x :: Int)
--
--   Associativity:
--   prop> \ a b c -> Joined a \/ (Joined b \/ Joined c) == (Joined a \/ Joined b) \/ (Joined (c :: Int))
--
--   Commutativity:
--   prop> \ a b -> Joined a \/ Joined b == Joined b \/ Joined (a :: Int)
--
--   Identity:
--   prop> \ a -> lower \/ Joined a == Joined (a :: Int)
--
--   Absorption:
--   prop> \ a -> upper \/ Joined a == (upper :: Joined Int)
newtype Joined a = Joined { getJoined :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Lower, Num, Ord, Read, Show, Traversable, Upper)

instance Ord a => Join (Joined a) where
  a \/ b
    | a <= b    = b
    | otherwise = a


newtype LessThan a = LessThan { getLessThan :: a }
  deriving (Enum, Eq, Foldable, Functor, Join, Num, Read, Show, Traversable)

instance (Eq a, Join a) => Ord (LessThan a) where
  compare a b
    | a == b      = EQ
    | a \/ b == b = LT
    | otherwise   = GT

  a <= b = a \/ b == b
