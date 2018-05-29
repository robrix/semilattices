{-# LANGUAGE DefaultSignatures, PolyKinds, TypeFamilies, TypeOperators #-}
-- | Upper bounds, related to 'Bounded', 'Join', 'Meet', and 'Ord'.
module Data.Semilattice.Upper where

import Data.Char
import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Type.Coercion
import Data.Type.Equality
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import System.Posix.Types

-- | The least upper bound of @s@.
--
--   Laws:
--
--   If @s@ is 'Bounded', we require 'upperBound' and 'maxBound' to agree:
--
-- @
-- 'upperBound' = 'maxBound'
-- @
--
--   If @s@ is a 'Meet' semilattice, 'upperBound' must be the identity of '/\':
--
-- @
-- 'upperBound' '\/' a = a
-- @
--
--   If @s@ is a 'Join' semilattice, 'upperBound' must be the absorbing element of '\/':
--
-- @
-- 'upperBound' '\/' a = 'upperBound'
-- @
--
--   If @s@ is 'Ord'ered, 'upperBound' must be at least as large as every terminating value:
--
-- @
-- 'compare' 'upperBound' a /= 'LT'
-- @
class Upper s where
  upperBound :: s
  default upperBound :: Bounded s => s
  upperBound = maxBound


-- Prelude
instance Upper ()

-- $
--
-- Bounded:
--
-- prop> upperBound == (maxBound :: Bool)
--
-- Identity of '/\':
--
-- prop> upperBound /\ a == (a :: Bool)
--
-- Absorbing element of '\/':
--
-- prop> upperBound \/ a == (upperBound :: Bool)
--
-- Ord:
--
-- prop> compare upperBound (a :: Bool) /= LT
instance Upper Bool
instance Upper Ordering
instance Upper Char
instance Upper Int
instance (Upper a, Upper b) => Upper (a, b) where upperBound = (upperBound, upperBound)
instance (Upper a, Upper b, Upper c) => Upper (a, b, c) where upperBound = (upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d) => Upper (a, b, c, d) where upperBound = (upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e) => Upper (a, b, c, d, e) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f) => Upper (a, b, c, d, e, f) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g) => Upper (a, b, c, d, e, f, g) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h) => Upper (a, b, c, d, e, f, g, h) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i) => Upper (a, b, c, d, e, f, g, h, i) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j) => Upper (a, b, c, d, e, f, g, h, i, j) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k) => Upper (a, b, c, d, e, f, g, h, i, j, k) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l) => Upper (a, b, c, d, e, f, g, h, i, j, k, l) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m, Upper n) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m, Upper n, Upper o) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound, upperBound)
instance Upper b => Upper (a -> b) where upperBound = const upperBound


-- Data.Char
instance Upper GeneralCategory

-- Data.Int
instance Upper Int8
instance Upper Int16
instance Upper Int32
instance Upper Int64

-- Data.Functor.Const
instance Upper a => Upper (Const a b) where upperBound = Const upperBound

-- Data.Functor.Identity
instance Upper a => Upper (Identity a) where upperBound = Identity upperBound

-- Data.Monoid
instance Upper All
instance Upper Any
instance Upper a => Upper (Product a) where upperBound = Product upperBound
instance Upper a => Upper (Sum a) where upperBound = Sum upperBound
instance Upper a => Upper (Dual a) where upperBound = Dual upperBound

-- Data.Proxy
instance Upper (Proxy a)

-- Data.Semigroup
instance Upper a => Upper (Semigroup.First a) where upperBound = Semigroup.First upperBound
instance Upper a => Upper (Semigroup.Last a) where upperBound = Semigroup.Last upperBound
instance Upper a => Upper (Max a) where upperBound = Max upperBound
instance Upper a => Upper (Min a) where upperBound = Min upperBound
instance Upper a => Upper (WrappedMonoid a) where upperBound = WrapMonoid upperBound

-- Data.Type.Coercion
instance Coercible a b => Upper (Coercion a b)

-- Data.Type.Equality
instance (a ~ b) => Upper (a :~: b)
instance (a ~~ b) => Upper (a :~~: b)

-- Data.Word
instance Upper Word8
instance Upper Word16
instance Upper Word32
instance Upper Word64

-- Foreign.C.Types
instance Upper CUIntMax
instance Upper CIntMax
instance Upper CUIntPtr
instance Upper CIntPtr
instance Upper CSigAtomic
instance Upper CWchar
instance Upper CSize
instance Upper CPtrdiff
instance Upper CBool
instance Upper CULLong
instance Upper CLLong
instance Upper CULong
instance Upper CLong
instance Upper CUInt
instance Upper CInt
instance Upper CUShort
instance Upper CShort
instance Upper CUChar
instance Upper CSChar
instance Upper CChar

-- Foreign.Ptr
instance Upper IntPtr
instance Upper WordPtr

-- GHC.Generics
instance Upper DecidedStrictness
instance Upper SourceStrictness
instance Upper SourceUnpackedness
instance Upper Associativity

-- System.Posix.Types
instance Upper Fd
instance Upper CKey
instance Upper CId
instance Upper CFsFilCnt
instance Upper CFsBlkCnt
instance Upper CClockId
instance Upper CBlkCnt
instance Upper CBlkSize
instance Upper CRLim
instance Upper CTcflag
instance Upper CUid
instance Upper CNlink
instance Upper CGid
instance Upper CSsize
instance Upper CPid
instance Upper COff
instance Upper CMode
instance Upper CIno
instance Upper CDev


-- $setup
-- >>> import Data.Semilattice.Join
-- >>> import Data.Semilattice.Meet
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Test.QuickCheck.Function
