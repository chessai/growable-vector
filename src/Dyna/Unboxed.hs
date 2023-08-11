{-# language
    ImportQualifiedPost
  , PackageImports
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}

-- |
--   = Purpose
--   A contiguous growable array type with heap-allocated contents, written @'Vec' s a@.
--
--   'Vec' has /O(1)/ indexing, amortized /O(1)/ 'push' (to the end), and /O(1)/ 'pop' (from the end).
--
--   == A note about type variable ordering
--   The type variables in this module will be ordered according to the following precedence list (with higher precedence meaning left-most in the forall-quantified type variables):
--
--     1. The 'PrimMonad' type variable, @m@
--     2. The 'PrimState' token, @s@
--     3. The element type, @a@
--     4. Everything else is unspecified (look at the forall)
--
--   This structuring is intended to document and ensure a uniform experience for users of @-XTypeApplications@.
--
--   == A note about ordering of arguments
--   The arguments of functions in this module will generally be ordered as follows:
--
--     1. The vector
--     2. The index (e.g. when reading/writing)
--     3. The element
--
--   As an example, 'write', which has all three of these, takes the vector, then the index, then the element.
--
--   As another example, 'push', which does not take an index, takes the vector, then the element.
--
--   Any function which accepts more than one of the types in the list, or accepts a type which is not in the list, has no guarantee about its argument order.
module Dyna.Unboxed
  ( Vec

  , new
  , withCapacity

  , length
  , capacity

  , reserve
  , reserveExact

  , shrinkToFit
  , shrinkTo

  , unsafeRead
  , read
  , unsafeWrite
  , write
  , push
  , pop
  , extend
  , fromFoldable

  , toList
  , toLiftedVector, toLiftedVectorWith
  , toPrimitiveVector, toPrimitiveVectorWith
  , toStorableVector, toStorableVectorWith

  , map, map', imap, imap'
  ) where

import Dyna qualified

import Control.Monad
import Control.Monad.Primitive
import Data.Coerce (coerce)
import Data.Primitive
import Foreign.Storable (Storable)
import Prelude hiding (length, read, map)
import Data.Vector qualified as LiftedVector
import Data.Vector.Primitive qualified as PrimitiveVector
import Data.Vector.Storable qualified as StorableVector

-- |
--   = Indexing
--   The 'Vec' type allows access to values by (0-based) index. An example will be more explicit:
--
--   >>> vec <- fromFoldable @_ @_ @Int [0, 2, 4, 6]
--   >>> two <- unsafeRead vec 1
--   >>> unsafeWrite vec 1 7
--   >>> seven <- unsafeRead vec 1
--   >>> two + seven
--   9
--
--   However, be careful: if you try to access an index which isn't in the 'Vec', your program will exhibit undefined behaviour (\"UB\"), either returning garbage or segfaulting. You cannot do this:
--
--   @
--   v <- fromFoldable [0, 2, 4, 6]
--   unsafeRead v 6 -- this is UB
--   @
--
--   If you want safe access, use 'read':
--
--   >>> vec <- fromFoldable @_ @_ @Int [0, 2, 4, 6]
--   >>> read vec 1
--   Just 2
--   >>> read vec 6
--   Nothing
--
--   = Capacity and reallocation
--   The capacity of a vector is the amount of space allocated for any future elements that will be added onto the vector. This is not to be confused with the /length/ of a vector, which specifies the number of actual elements within the vector. If a vector's length exceeds its capacity, its capacity will automatically be increased, but its elements will have to be reallocated.
--
--   For example, a vector with capacity 10 and length 0 would be an empty vector with space for 10 more elements. Pushing 10 or fewer elements onto the vector will not change its capacity or cause reallocation to occur. However, if the vector's length is increased to 11, it will have to reallocate, which can be slow. For this reason, it is recommended to use 'withCapacity' whenever possible to specify how big the vector is expected to get.
--
--   = Guarantees
--   'Vec' is a (pointer, capacity, length) triplet. The pointer
--   will never be null.
--
--   Because of the semantics of the GHC runtime, creating a new vector will always allocate. In particular, if you construct a 'MutablePrimArray'-backed 'Vec' with capacity 0 via @'new' 0@, @'fromFoldable' []@, or @'shrinkToFit'@ on an empty 'Vec', the 16-byte header to GHC 'ByteArray#' will be allocated, but nothing else (for the curious: this is needed for 'sameMutableByteArray#' to work). Similarly, if you store zero-sized types inside such a 'Vec', it will not allocate space for them. /Note that in this case the 'Vec' may not report a 'capacity' of 0/. 'Vec' will allocate if and only if @'sizeOf (undefined :: a) '*' 'capacity' '>' 0@.
--
--   If a 'Vec' /has/ allocated memory, then the memory it points to is on the heap (as defined by GHC's allocator), and its pointer points to 'length' initialised, contiguous elements in order (i.e. what you would see if you turned it into a list), followed by @'maxCapacity' '-' 'length'@ logically uninitialised, contiguous elements.
--
--   'Vec' will never automatically shrink itself, even if completely empty. This ensures no unnecessary allocations or deallocations occur. Emptying a 'Vec' and then filling it back up to the same 'length' should incur no calls to the allocator. If you wish to free up unused memory, use 'shrinkToFit'.
--
--   'push' and 'insert' will never (re)allocate if the reported capacity is sufficient. 'push' and 'insert' /will/ (re)allocate if @'length' '==' 'capacity'@. That is, the reported capacity is completely accurate, and can be relied on. Bulk insertion methods /may/ reallocate, even when not necessary.
--
--   'Vec' does not guarantee any particular growth strategy when reallocating when full, nor when 'reserve' is called. The strategy is basic and may prove desirable to use a non-constant growth factor. Whatever strategy is used will of course guarantee /O(1)/ amortised push.
--
--   'fromFoldable' and 'withCapacity' will produce a 'Vec' with exactly the requested capacity.
--
--   'Vec' will not specifically overwrite any data that is removed from it, but also won't specifically preserve it. Its uninitialised memory is scratch space that it may use however it wants. It will generally just do whatever is most efficient or otherwise easy to implement. Do not rely on removed data to be erased for security purposes. Even if a 'Vec' drops out of scope, its buffer may simply be reused by another 'Vec'. Even if you zero a 'Vec's memory first, this may not actually happen when you think it does because of Garbage Collection.
newtype Vec s a = Vec (Dyna.Vec PrimArray s a)

-- | \(O(1)\). Constructs a new, empty @'Vec' s a'@.
--
-- >>> vec <- new @_ @_ @Int
-- >>> assertM (== 0) (length vec)
-- >>> assertM (== 0) (capacity vec)
new :: forall m s a. (MonadPrim s m, Prim a)
  => m (Vec s a)
new = fmap coerce Dyna.new

-- | \(O(1)\). Constructs a new, empty @'Vec' s a'@.
--
--   The vector will be able to hold exactly @capacity@
--   elements without reallocating. It is important to
--   note that althrough the returned vector has the
--   /capacity/ specified, with vector will have a zero
--   /length/.
--
-- >>> vec <- withCapacity @_ @_ @Int 10
--
-- -- The vector contains no items, even though it has
-- -- capacity for more
-- >>> assertM (== 0) (length vec)
-- >>> assertM (== 10) (capacity vec)
--
-- -- These are all done without reallocating...
-- >>> forM_ [1..10] $ \i -> push vec i
-- >>> assertM (== 10) (length vec)
-- >>> assertM (== 10) (capacity vec)
--
-- -- ..but this may make the vector reallocate
-- >>> push vec 11
--
-- >>> assertM (== 11) (length vec)
-- >>> assertM (>= 11) (capacity vec)
withCapacity :: forall m s a. (MonadPrim s m, Prim a)
  => Word -- ^ capacity
  -> m (Vec s a)
withCapacity sz = fmap coerce (Dyna.withCapacity sz)

-- | \(O(1)\). Returns the number of elements in the vector.
--
length :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m Word
length vec = Dyna.length (coerce vec)

-- | \(O(1)\). Returns the maximum number of elements the vector
--   can hold without reallocating.
--
-- >>> vec <- withCapacity @_ @_ @Word 10
-- >>> push vec 42
-- >>> assertM (== 10) (capacity vec)
capacity :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m Word
capacity vec = Dyna.capacity (coerce vec)

-- | \(O(n)\). Reserves the minimum capacity for exactly @additional@
--   more elements to be inserted in the given @'Vec' s a@.
--   After calling 'reserveExact', capacity will be greater
--   than or equal to @length + additional@. Does nothing if
--   the capacity is already sufficient.
--
--   Capacity cannot be relied upon to be precisely minimal. Prefer 'reserve'
--   if future insertions are expected.
--
-- >>> vec <- fromFoldable @_ @_ @Int @_ [1]
-- >>> reserveExact vec 10
-- >>> assertM (>= 11) (capacity vec)
reserveExact :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> m ()
reserveExact vec additional = Dyna.reserveExact (coerce vec) additional

-- | \(O(n)\). Reserves capacity for at least @additional@ more elements
--   to be inserted in the given @'Vec' s a@. The collection
--   may reserve more space to avoid frequent reallocations.
--   After calling 'reserve', capacity will be greater than or
--   equal to @length + additional@. Does nothing if capacity
--   is already sufficient.
--
-- >>> vec <- fromFoldable @_ @_ @Int @_ [1]
-- >>> reserve vec 10
-- >>> assertM (>= 11) (capacity vec)
reserve :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> m ()
reserve vec additional = Dyna.reserve (coerce vec) additional

-- | \(O(n)\). Shrinks the capacity of the vector as much as possible.
--
-- >>> vec <- withCapacity @_ @_ @Int 10
-- >>> extend vec [1, 2, 3]
-- >>> assertM (== 10) (capacity vec)
-- >>> shrinkToFit vec
-- >>> assertM (== 3) (capacity vec)
shrinkToFit :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m ()
shrinkToFit vec = Dyna.shrinkToFit (coerce vec)

-- | \(O(n)\). Shrinks the capacity of the vector with a lower bound.
--
--   The capacity will remain at least as large as both the length and the
--   supplied value.
--
--   If the current capacity is less than the lower limit, this is a no-op.
--
-- >>> vec <- withCapacity @_ @_ @Int 10
-- >>> extend vec [1, 2, 3]
-- >>> assertM (== 10) (capacity vec)
-- >>> shrinkTo vec 4
-- >>> assertM (>= 4) (capacity vec)
-- >>> shrinkTo vec 0
-- >>> assertM (>= 3) (capacity vec)
shrinkTo :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> m ()
shrinkTo vec minCap = Dyna.shrinkTo (coerce vec) minCap

-- | \(O(1)\). Return the element at the given position.
--
--   If the element index is out of bounds, this function will segfault.
--
--   The bounds are defined as [0, length).
--
--   This function is intended to be used in situations where you have
--   manually proved that your indices are within bounds, and you don't want to
--   repeatedly pay for bounds checking.
--
--   Consider using 'read' instead.
unsafeRead :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> m a
unsafeRead vec n = Dyna.unsafeRead (coerce vec) n

-- | \(O(1)\). Return the element at the given position, or 'Nothing' if the index is out
--   of bounds.
--
--   The bounds are defined as [0, length).
read :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> m (Maybe a)
read vec n = Dyna.read (coerce vec) n

-- | \(O(1)\). Write a value to the vector at the given position.
--
--   If the index is out of bounds, this function will segfault.
--
--   The bounds are defined as [0, length).
--   If you want to add an element past @length - 1@, use `push` or `extend`, which can
--   intelligently handle resizes.
--
--   This function is intended to be used in situations where you have
--   manually proved that your indices are within bounds, and you don't want to
--   repeatedly pay for bounds checking.
--
--   Consider using 'write' instead.
unsafeWrite :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> a
  -> m ()
unsafeWrite vec n x = Dyna.unsafeWrite (coerce vec) n x

-- | \(O(1)\). Write a value to the vector at the given position.
--
--   If the index is in bounds, this returns @'Just' ()@.
--   If the index is out of bounds, this returns 'Nothing'.
--
--   The bounds are defined as [0, length).
--   If you want to add an element past @length - 1@, use `push` or `extend`, which can
--   intelligently handle resizes.
write :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> Word
  -> a
  -> m (Maybe ())
write vec n x = Dyna.write (coerce vec) n x

-- | Amortised \(O(1)\). Appends an element to the vector.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1, 2]
-- >>> push vec 3
-- >>> assertM (== [1, 2, 3]) (toList vec)
push :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> a
  -> m ()
push vec x = Dyna.push (coerce vec) x

-- | \(O(1)\). Removes the last element from a vector and returns it, or 'Nothing' if it
--   is empty.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1, 2, 3]
-- >>> assertM (== Just 3) (pop vec)
-- >>> assertM (== [1, 2]) (toList vec)
pop :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m (Maybe a)
pop vec = Dyna.pop (coerce vec)

-- | \(O(m)\). Extend the vector with the elements of some 'Foldable' structure.
--
-- >>> vec <- fromFoldable @_ @_ @Char ['a', 'b', 'c']
-- >>> extend vec ['d', 'e', 'f']
-- >>> assertM (== "abcdef") (toList vec)
extend :: forall m s a t. (MonadPrim s m, Prim a, Foldable t)
  => Vec s a
  -> t a
  -> m ()
extend vec xs = Dyna.extend (coerce vec) xs

-- | \(O(n)\). Create a vector with the elements of some 'Foldable' structure.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1..10]
-- >>> assertM (== [1..10]) (toList vec)
fromFoldable :: forall m s a t. (MonadPrim s m, Prim a, Foldable t)
  => t a
  -> m (Vec s a)
fromFoldable xs = fmap coerce (Dyna.fromFoldable xs)

-- | \(O(n)\). Create a list with the elements of the vector.
--
-- >>> vec <- new @_ @_ @Int
-- >>> extend vec [1..5]
-- >>> toList vec
-- [1,2,3,4,5]
toList :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m [a]
toList vec = Dyna.toList (coerce vec)

-- | \(O(n)\). Create a Primitive 'PrimitiveVector.Vector' copy of a vector.
toPrimitiveVector :: forall m s a. (MonadPrim s m, Prim a, Prim a)
  => Vec s a
  -> m (PrimitiveVector.Vector a)
toPrimitiveVector vec = Dyna.toPrimitiveVector (coerce vec)

-- | \(O(n)\). Create a Storable 'StorableVector.Vector' copy of a vector.
toStorableVector :: forall m s a. (MonadPrim s m, Prim a, Storable a)
  => Vec s a
  -> m (StorableVector.Vector a)
toStorableVector vec = Dyna.toStorableVector (coerce vec)

-- | \(O(n)\). Create a lifted 'LiftedVector.Vector' copy of a vector.
toLiftedVector :: forall m s a. (MonadPrim s m, Prim a)
  => Vec s a
  -> m (LiftedVector.Vector a)
toLiftedVector vec = Dyna.toLiftedVector (coerce vec)

-- | \(O(n)\). Create a Primitive 'PrimitiveVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toPrimitiveVectorWith :: forall m s a b. (MonadPrim s m, Prim a, Prim a, Prim b)
  => (Word -> a -> m b)
  -> Vec s a
  -> m (PrimitiveVector.Vector b)
toPrimitiveVectorWith f vec = Dyna.toPrimitiveVectorWith f (coerce vec)

-- | \(O(n)\). Create a Storable 'StorableVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toStorableVectorWith :: forall m s a b. (MonadPrim s m, Prim a, Storable b)
  => (Word -> a -> m b)
  -> Vec s a
  -> m (StorableVector.Vector b)
toStorableVectorWith f vec = Dyna.toStorableVectorWith f (coerce vec)

-- | \(O(n)\). Create a lifted 'LiftedVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toLiftedVectorWith :: forall m s a b. (MonadPrim s m, Prim a)
  => (Word -> a -> m b)
  -> Vec s a
  -> m (LiftedVector.Vector b)
toLiftedVectorWith f vec = Dyna.toLiftedVectorWith f (coerce vec)

-- | \(O(n)\). Map over an array, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1..10]
-- >>> map (+ 1) vec
-- >>> assertM (== [2, 3 .. 11]) (toList vec)
map :: forall m s a. (MonadPrim s m, Prim a)
  => (a -> a)
  -> Vec s a
  -> m ()
map f vec = Dyna.map f (coerce vec)

-- | \(O(n)\). Map strictly over an array, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1..10]
-- >>> map' (* 2) vec
-- >>> assertM (== [2, 4 .. 20]) (toList vec)
map' :: forall m s a. (MonadPrim s m, Prim a)
  => (a -> a)
  -> Vec s a
  -> m ()
map' f vec = Dyna.map' f (coerce vec)

-- | \(O(n)\). Map over an array with a function that takes the index and its
--   corresponding element as input, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1..10]
-- >>> imap (\ix el -> ix + 1) vec
-- >>> assertM (== zipWith (+) [0..] [1..10]) (toList vec)
imap :: forall m s a. (MonadPrim s m, Prim a)
  => (Word -> a -> a)
  -> Vec s a
  -> m ()
imap f vec = Dyna.imap f (coerce vec)

-- | \(O(n)\). Map strictly over an array with a function that takes
--   the index and its corresponding element as input, modifying the
--   elements in place.
--
-- >>> vec <- fromFoldable @_ @_ @Int [1..10]
-- >>> imap' (\ix el -> ix + 1) vec
-- >>> assertM (== zipWith (+) [0..] [1..10]) (toList vec)
imap' :: forall m s a. (MonadPrim s m, Prim a)
  => (Word -> a -> a)
  -> Vec s a
  -> m ()
imap' f vec = Dyna.imap' f (coerce vec)
