{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | Join semilattices, related to 'Lower' and 'Data.Semilattice.Upper.Upper'.
module Data.Semilattice.Join
( Join(..)
, Joining(..)
, LessThan(..)
) where

import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Semigroup
import Data.Semilattice.Lower
import Data.Set as Set

-- | A join semilattice is an idempotent commutative semigroup.
class Join s where
  -- | The join operation.
  --
  --   Laws:
  --
  --   Idempotence:
  --
  -- @
  -- x '\/' x = x
  -- @
  --
  --   Associativity:
  --
  -- @
  -- a '\/' (b '\/' c) = (a '\/' b) '\/' c
  -- @
  --
  --   Commutativity:
  --
  -- @
  -- a '\/' b = b '\/' a
  -- @
  --
  --   Additionally, if @s@ has a 'Lower' bound, then 'lowerBound' must be its identity:
  --
  -- @
  -- 'lowerBound' '\/' a = a
  -- a '\/' 'lowerBound' = a
  -- @
  --
  --   If @s@ has an 'Data.Semilattice.Upper.Upper' bound, then 'Data.Semilattice.Upper.upperBound' must be its absorbing element:
  --
  -- @
  -- 'Data.Semilattice.Upper.upperBound' '\/' a = 'Data.Semilattice.Upper.upperBound'
  -- a '\/' 'Data.Semilattice.Upper.upperBound' = 'Data.Semilattice.Upper.upperBound'
  -- @
  (\/) :: s -> s -> s

  infixr 6 \/


-- Prelude

instance Join () where
  _ \/ _ = ()

-- | Boolean disjunction forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: Bool)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: Bool)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: Bool)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: Bool)
--
--   Absorption:
--
--   prop> upperBound \/ a == (upperBound :: Bool)
instance Join Bool where
  (\/) = (||)

-- | Orderings form a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: Ordering)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: Ordering)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: Ordering)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: Ordering)
--
--   Absorption:
--
--   prop> upperBound \/ a == (upperBound :: Ordering)
instance Join Ordering where
  GT \/ _ = GT
  _ \/ GT = GT
  LT \/ b = b
  a \/ LT = a
  _ \/ _ = EQ

-- | Functions with semilattice codomains form a semilattice.
--
--   Idempotence:
--
--   prop> \ (Fn x) -> x \/ x ~= (x :: Int -> Bool)
--
--   Associativity:
--
--   prop> \ (Fn a) (Fn b) (Fn c) -> a \/ (b \/ c) ~= (a \/ b) \/ (c :: Int -> Bool)
--
--   Commutativity:
--
--   prop> \ (Fn a) (Fn b) -> a \/ b ~= b \/ (a :: Int -> Bool)
--
--   Identity:
--
--   prop> \ (Fn a) -> lowerBound \/ a ~= (a :: Int -> Bool)
--
--   Absorption:
--
--   prop> \ (Fn a) -> upperBound \/ a ~= (upperBound :: Int -> Bool)
instance Join b => Join (a -> b) where
  f \/ g = (\/) <$> f <*> g


-- Data.Semigroup

-- | The least upperBound bound gives rise to a join semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: Max Int)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: Max Int)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: Max Int)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: Max Int)
--
--   Absorption:
--
--   prop> upperBound \/ a == (upperBound :: Max Int)
instance Ord a => Join (Max a) where
  (\/) = (<>)


-- containers

-- | IntMap union with 'Join'able values forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: IntMap (Set Char))
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: IntMap (Set Char))
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: IntMap (Set Char))
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: IntMap (Set Char))
instance Join a => Join (IntMap a) where
  (\/) = IntMap.unionWith (\/)

-- | IntSet union forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: IntSet)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: IntSet)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: IntSet)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: IntSet)
instance Join IntSet where
  (\/) = IntSet.union

-- | Map union with 'Join'able values forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: Map Char (Set Char))
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: Map Char (Set Char))
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: Map Char (Set Char))
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: Map Char (Set Char))
instance (Ord k, Join a) => Join (Map k a) where
  (\/) = Map.unionWith (\/)

-- | Set union forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: Set Char)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: Set Char)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: Set Char)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: Set Char)
instance Ord a => Join (Set a) where
  (\/) = Set.union


-- unordered-containers

-- | HashMap union with 'Join'able values forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: HashMap Char (Set Char))
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: HashMap Char (Set Char))
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: HashMap Char (Set Char))
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: HashMap Char (Set Char))
instance (Eq k, Hashable k, Join a) => Join (HashMap k a) where
  (\/) = HashMap.unionWith (\/)

-- | HashSet union forms a semilattice.
--
--   Idempotence:
--
--   prop> x \/ x == (x :: HashSet Char)
--
--   Associativity:
--
--   prop> a \/ (b \/ c) == (a \/ b) \/ (c :: HashSet Char)
--
--   Commutativity:
--
--   prop> a \/ b == b \/ (a :: HashSet Char)
--
--   Identity:
--
--   prop> lowerBound \/ a == (a :: HashSet Char)
instance (Eq a, Hashable a) => Join (HashSet a) where
  (\/) = HashSet.union


-- | A 'Semigroup' for any 'Join' semilattice.
--
--   If the semilattice has a 'Lower' bound, there is additionally a 'Monoid' instance.
newtype Joining a = Joining { getJoining :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Join, Lower, Num, Ord, Read, Show, Traversable)

-- | 'Joining' '<>' is associative.
--
--   prop> \ a b c -> Joining a <> (Joining b <> Joining c) == (Joining a <> Joining b) <> Joining (c :: IntSet)
instance Join a => Semigroup (Joining a) where
  (<>) = (\/)

-- | 'Joining' 'mempty' is the left- and right-identity.
--
--   prop> \ x -> let (l, r) = (mappend mempty (Joining x), mappend (Joining x) mempty) in l == Joining x && r == Joining (x :: IntSet)
instance (Lower a, Join a) => Monoid (Joining a) where
  mappend = (<>)
  mempty = lowerBound


-- | 'Join' semilattices give rise to a partial 'Ord'ering.
newtype LessThan a = LessThan { getLessThan :: a }
  deriving (Enum, Eq, Foldable, Functor, Join, Num, Read, Show, Traversable)

-- | NB: This is not in general a total ordering.
instance (Eq a, Join a) => Ord (LessThan a) where
  compare a b
    | a == b      = EQ
    | a \/ b == b = LT
    | otherwise   = GT

  a <= b = a \/ b == b


-- $setup
-- >>> import Data.Semilattice.Upper
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Function
-- >>> import Test.QuickCheck.Instances.UnorderedContainers ()
-- >>> instance Arbitrary a => Arbitrary (Max a) where arbitrary = Max <$> arbitrary
-- >>> :{
-- infix 4 ~=
-- f ~= g = (==) <$> f <*> g
-- :}
