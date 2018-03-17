{-# LANGUAGE DefaultSignatures, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Upper where

import Data.Char
import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Set as Set
import Data.Type.Coercion
import Data.Type.Equality
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics
import System.Posix.Types

class Upper s where
  -- | The least upper bound of @s@.
  --
  --   Laws:
  --
  --   If @s@ is 'Bounded', we require 'upper' and 'maxBound' to agree:
  --
  --   > upper = maxBound
  --
  --   If @s@ is a 'Meet', 'upper' must be the identity of '(/\)':
  --
  --   > upper \/ a = a
  --
  --   If @s@ is 'Ord'ered, 'upper' must be at least as large as every terminating value:
  --
  --   > compare upper a /= LT
  upper :: s
  default upper :: Bounded s => s
  upper = maxBound


-- Prelude
instance Upper ()
instance Upper Bool
instance Upper Ordering
instance Upper Char
instance Upper Int
instance (Upper a, Upper b) => Upper (a, b) where upper = (upper, upper)
instance (Upper a, Upper b, Upper c) => Upper (a, b, c) where upper = (upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d) => Upper (a, b, c, d) where upper = (upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e) => Upper (a, b, c, d, e) where upper = (upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f) => Upper (a, b, c, d, e, f) where upper = (upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g) => Upper (a, b, c, d, e, f, g) where upper = (upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h) => Upper (a, b, c, d, e, f, g, h) where upper = (upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i) => Upper (a, b, c, d, e, f, g, h, i) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j) => Upper (a, b, c, d, e, f, g, h, i, j) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k) => Upper (a, b, c, d, e, f, g, h, i, j, k) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l) => Upper (a, b, c, d, e, f, g, h, i, j, k, l) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m, Upper n) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)
instance (Upper a, Upper b, Upper c, Upper d, Upper e, Upper f, Upper g, Upper h, Upper i, Upper j, Upper k, Upper l, Upper m, Upper n, Upper o) => Upper (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where upper = (upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper, upper)

instance Upper (Maybe a) where upper = Nothing
instance Upper [a] where upper = []


-- Data.Char
instance Upper GeneralCategory

-- Data.Int
instance Upper Int8
instance Upper Int16
instance Upper Int32
instance Upper Int64

-- Data.Functor.Const
instance Upper a => Upper (Const a b) where upper = Const upper

-- Data.Functor.Identity
instance Upper a => Upper (Identity a) where upper = Identity upper

-- Data.Monoid
instance Upper All
instance Upper Any
instance Upper a => Upper (Product a) where upper = Product upper
instance Upper a => Upper (Sum a) where upper = Sum upper
instance Upper a => Upper (Dual a) where upper = Dual upper

-- Data.Proxy
instance Upper (Proxy a)

-- Data.Semigroup
instance Upper a => Upper (Semigroup.First a) where upper = Semigroup.First upper
instance Upper a => Upper (Semigroup.Last a) where upper = Semigroup.Last upper
instance Upper a => Upper (Max a) where upper = Max upper
instance Upper a => Upper (Min a) where upper = Min upper
instance Upper a => Upper (WrappedMonoid a) where upper = WrapMonoid upper

-- Data.Type.Coercion
instance Coercible a b => Upper (Coercion a b)

-- Data.Type.Equality
instance a ~ b => Upper (a :~: b)
instance a ~~ b => Upper (a :~~: b)

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

-- containers
instance Upper (IntMap a) where upper = IntMap.empty
instance Upper (Map k a) where upper = Map.empty
instance Upper (Set a) where upper = Set.empty
