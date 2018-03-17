{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semilattice.Meet where

import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Semigroup
import Data.Semilattice.Lower
import Data.Semilattice.Upper
import Data.Set as Set

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Function
-- >>> import Test.QuickCheck.Instances.UnorderedContainers ()
-- >>> :{
-- let infix 4 ~=
--     f ~= g = (==) <$> f <*> g
-- :}

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


-- Prelude

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

-- | Functions with semilattice codomains form a semilattice.
--
--   Idempotence:
--   prop> \ (Fn x) -> x /\ x ~= (x :: Int -> Bool)
--
--   Associativity:
--   prop> \ (Fn a) (Fn b) (Fn c) -> a /\ (b /\ c) ~= (a /\ b) /\ (c :: Int -> Bool)
--
--   Commutativity:
--   prop> \ (Fn a) (Fn b) -> a /\ b ~= b /\ (a :: Int -> Bool)
--
--   Identity:
--   prop> \ (Fn a) -> upper /\ a ~= (a :: Int -> Bool)
--
--   Absorption:
--   prop> \ (Fn a) -> lower /\ a ~= (lower :: Int -> Bool)
instance Meet b => Meet (a -> b) where
  f /\ g = (/\) <$> f <*> g


-- Data.Semigroup

instance Ord a => Meet (Min a) where
  (/\) = (<>)


-- containers

-- | IntMap union with 'Meet'able values forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: IntMap (Set Char))
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: IntMap (Set Char))
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: IntMap (Set Char))
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: IntMap (Set Char))
instance Meet a => Meet (IntMap a) where
  (/\) = IntMap.intersectionWith (/\)

-- | IntSet intersection forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: IntSet)
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: IntSet)
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: IntSet)
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: IntSet)
instance Meet IntSet where
  (/\) = IntSet.intersection

-- | Map union with 'Meet'able values forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: Map Char (Set Char))
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: Map Char (Set Char))
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: Map Char (Set Char))
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: Map Char (Set Char))
instance (Ord k, Meet a) => Meet (Map k a) where
  (/\) = Map.intersectionWith (/\)

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
  (/\) = Set.intersection


-- unordered-containers

-- | HashMap union with 'Meet'able values forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: HashMap Char (Set Char))
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: HashMap Char (Set Char))
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: HashMap Char (Set Char))
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: HashMap Char (Set Char))
instance (Eq k, Hashable k, Meet a) => Meet (HashMap k a) where
  (/\) = HashMap.intersectionWith (/\)

-- | HashSet intersection forms a semilattice.
--
--   Idempotence:
--   prop> \ x -> x /\ x == (x :: HashSet Char)
--
--   Associativity:
--   prop> \ a b c -> a /\ (b /\ c) == (a /\ b) /\ (c :: HashSet Char)
--
--   Commutativity:
--   prop> \ a b -> a /\ b == b /\ (a :: HashSet Char)
--
--   Absorption:
--   prop> \ a -> lower /\ a == (lower :: HashSet Char)
instance (Eq a, Hashable a) => Meet (HashSet a) where
  (/\) = HashSet.intersection


-- | A 'Semigroup' for any 'Meet' semilattice.
--
--   If the semilattice has an 'Upper' bound, there is additionally a 'Monoid' instance.
newtype Meeting a = Meeting { getMeeting :: a }
  deriving (Bounded, Enum, Eq, Foldable, Functor, Meet, Num, Ord, Read, Show, Traversable, Upper)

-- | 'Meeting' '(<>)' is associative.
--
--   prop> \ a b c -> Meeting a <> (Meeting b <> Meeting c) == (Meeting a <> Meeting b) <> Meeting (c :: IntSet)
instance Meet a => Semigroup (Meeting a) where
  (<>) = (/\)

-- | 'Meeting' 'mempty' is the left- and right-identity.
--
--   prop> \ x -> let (l, r) = (mappend mempty (Meeting x), mappend (Meeting x) mempty) in l == Meeting x && r == Meeting (x :: Bool)
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
    | compare a b == LT = a
    | otherwise         = b


newtype GreaterThan a = GreaterThan { getGreaterThan :: a }
  deriving (Enum, Eq, Foldable, Functor, Meet, Num, Read, Show, Traversable)

instance (Eq a, Meet a) => Ord (GreaterThan a) where
  compare a b
    | a == b      = EQ
    | a /\ b == a = LT
    | otherwise   = GT

  a <= b = a /\ b == a
