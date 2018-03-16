module Data.Meet where

import Data.Semigroup
import qualified Data.Set as Set

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
  --   Additionally, if @s@ has an 'Upper' bound, then 'top' must be its left- and right-identity.
  --
  --   > top /\ a = a
  --   > a /\ top = a
  (/\) :: s -> s -> s

  infixr 7 /\


instance Meet () where
  _ /\ _ = ()

instance Meet Bool where
  (/\) = (&&)

instance Ord a => Meet (Min a) where
  (/\) = (<>)

instance Ord a => Meet (Set.Set a) where
  (/\) = Set.intersection
