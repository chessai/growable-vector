{-# language
    BangPatterns
  , CPP
  , FlexibleInstances
  , ImportQualifiedPost
  , MagicHash
  , MultiParamTypeClasses
  , MultiWayIf
  , NamedFieldPuns
  , PackageImports
  , PolyKinds
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UnboxedSums
  , UnboxedTuples
#-}

-- |
--   = Purpose
--   A contiguous growable array type with heap-allocated contents, written @'Vec' arr s a@.
--
--   'Vec' has /O(1)/ indexing, amortised /O(1)/ 'push' (to the end), and /O(1)/ 'pop' (from the end).
--
--   == A note about representation
--   The type exposed by this module is polymorphic in the type of array backing the data structure, functionality
--   which is provided by the [contiguous](https://hackage.haskell.org/package/contiguous-0.6.3.0) library. There are
--   modules providing monomorphised variants for better type inference. They are:
--
--   * "GrowableVector.Lifted": backed by 'Data.Primitive.Array'; for storing lifted
--     elements. This suits most Haskell types.
--   * "GrowableVector.Lifted.Small": backed by 'Data.Primitive.SmallArray'; also for
--     storing lifted elements, but more efficient for <= 128 elements.
--   * "GrowableVector.Unlifted": backed by 'Data.Primitive.Unlifted.Array.UnliftedArray'; for storing elements which can be unlifted (i.e.,
--     non-thunk pointers; e.g., 'ByteArray', 'Control.Concurrent.MVar', 'Data.IORef.IORef'). See the
--     'Data.Primitive.Unlifted.Class.PrimUnlifted' typeclass for more details.
--   * "GrowableVector.Unboxed": backed by 'Data.Primitive.PrimArray'; for storing elements which can be completely unboxed
--     (i.e., non-thunk, non-pointers; e.g., 'Int', 'Word', 'Char'). See
--     the 'Prim' typeclass for more details.
--
--   == A note about type variable ordering
--   The type variables in this module will be ordered according to the following precedence list (with higher precedence meaning left-most in the forall-quantified type variables):
--
--     1. The 'PrimMonad' type variable, @m@
--     2. The array type, @arr@
--     3. The 'PrimState' token, @s@
--     4. The element type, @a@
--     5. Everything else is unspecified (look at the forall)
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
module GrowableVector
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
  -- , insert
  , extend
  , fromFoldable

  , toList
  , toLiftedVector, toLiftedVectorWith
  , toPrimitiveVector, toPrimitiveVectorWith
  , toStorableVector, toStorableVectorWith

  , map, map', imap, imap'
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable (traverse_)
import Data.Foldable qualified as F
import Data.Primitive
import Data.Primitive.Contiguous (Contiguous, Element, Mutable)
import Data.Primitive.Contiguous qualified as C
import GHC.Float (int2Double)
import Foreign.Storable (Storable)
import Prelude hiding (length, read, map)
import Data.Vector qualified as LiftedVector
import Data.Vector.Primitive qualified as PrimitiveVector
import Data.Vector.Storable qualified as StorableVector

-- |
--   = Indexing
--   The 'Vec' type allows access to values by (0-based) index. An example will be more explicit:
--
--   >>> vec <- fromFoldable @_ @Array @_ @Int [0, 2, 4, 6]
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
--   >>> vec <- fromFoldable @_ @Array @_ @Int [0, 2, 4, 6]
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
--   Because of the semantics of the GHC runtime, creating a new vector will always allocate. In particular, if you construct a 'MutablePrimArray'-backed 'Vec' with capacity 0 via @'new' 0@, @'fromFoldable' []@, or @'shrinkToFit'@ on an empty 'Vec', the 16-byte header to GHC 'ByteArray#' will be allocated, but nothing else (for the curious: this is needed for 'GHC.Exts.sameMutableByteArray#' to work). Similarly, if you store zero-sized types inside such a 'Vec', it will not allocate space for them. /Note that in this case the 'Vec' may not report a 'capacity' of 0/. 'Vec' will allocate if and only if @'sizeOf (undefined :: a) '*' 'capacity' '>' 0@.
--
--   If a 'Vec' /has/ allocated memory, then the memory it points to is on the heap (as defined by GHC's allocator), and its pointer points to 'length' initialised, contiguous elements in order (i.e. what you would see if you turned it into a list), followed by @'capacity' '-' 'length'@ logically uninitialised, contiguous elements.
--
--   'Vec' will never automatically shrink itself, even if completely empty. This ensures no unnecessary allocations or deallocations occur. Emptying a 'Vec' and then filling it back up to the same 'length' should incur no calls to the allocator. If you wish to free up unused memory, use 'shrinkToFit'.
--
--   'push' will never (re)allocate if the reported capacity is sufficient. 'push' /will/ (re)allocate if @'length' '==' 'capacity'@. That is, the reported capacity is completely accurate, and can be relied on. Bulk insertion methods /may/ reallocate, even when not necessary.
--
--   'Vec' does not guarantee any particular growth strategy when reallocating when full, nor when 'reserve' is called. The strategy is basic and may prove desirable to use a non-constant growth factor. Whatever strategy is used will of course guarantee /O(1)/ amortised push.
--
--   'fromFoldable' and 'withCapacity' will produce a 'Vec' with exactly the requested capacity.
--
--   'Vec' will not specifically overwrite any data that is removed from it, but also won't specifically preserve it. Its uninitialised memory is scratch space that it may use however it wants. It will generally just do whatever is most efficient or otherwise easy to implement. Do not rely on removed data to be erased for security purposes. Even if a 'Vec' drops out of scope, its buffer may simply be reused by another 'Vec'. Even if you zero a 'Vec's memory first, this may not actually happen when you think it does because of Garbage Collection.
data Vec arr s a = Vec
  { len :: {-# unpack #-} !(MutVar s Word)
  , buf :: {-# unpack #-} !(MutVar s (Mutable arr s a))
  }

-- | \(O(1)\). Constructs a new, empty @'Vec' arr s a@.
--
-- >>> vec <- new @_ @Array @_ @Int
-- >>> assertM (== 0) (length vec)
-- >>> assertM (== 0) (capacity vec)
new :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => m (Vec arr s a)
new = withCapacity 0

-- | \(O(1)\). Constructs a new, empty @'Vec' arr s a@.
--
--   The vector will be able to hold exactly @capacity@
--   elements without reallocating. It is important to
--   note that althrough the returned vector has the
--   /capacity/ specified, with vector will have a zero
--   /length/.
--
-- >>> vec <- withCapacity @_ @SmallArray @_ @Int 10
-- -- The vector contains no items, even though it has capacity for more
-- >>> assertM (== 0) (length vec)
-- >>> assertM (== 10) (capacity vec)
-- -- These are all done without reallocating...
-- >>> forM_ [1..10] $ \i -> push vec i
-- >>> assertM (== 10) (length vec)
-- >>> assertM (== 10) (capacity vec)
-- -- ..but this may make the vector reallocate
-- >>> push vec 11
-- >>> assertM (== 11) (length vec)
-- >>> assertM (>= 11) (capacity vec)
withCapacity :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Word -- ^ capacity
  -> m (Vec arr s a)
withCapacity sz = Vec
  <$> newMutVar 0
  <*> (newMutVar =<< cNew sz)

-- | \(O(1)\). Returns the number of elements in the vector.
--
length :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m Word
length vec = readMutVar (len vec)

-- | \(O(1)\). Returns the maximum number of elements the vector
--   can hold without reallocating.
--
-- >>> vec <- withCapacity @_ @PrimArray @_ @Word 10
-- >>> push vec 42
-- >>> assertM (== 10) (capacity vec)
capacity :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m Word
capacity vec = do
  internal_max_capacity vec

-- | \(O(n)\). Reserves the minimum capacity for exactly @additional@
--   more elements to be inserted in the given @'Vec' arr s a@.
--   After calling 'reserveExact', capacity will be greater
--   than or equal to @length + additional@. Does nothing if
--   the capacity is already sufficient.
--
--   Capacity cannot be relied upon to be precisely minimal. Prefer 'reserve'
--   if future insertions are expected.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int @_ [1]
-- >>> reserveExact vec 10
-- >>> assertM (>= 11) (capacity vec)
reserveExact :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m ()
reserveExact vec additional = do
  cap <- capacity vec
  when (additional > cap) $ do
    oldBuf <- internal_vector vec
    oldSize <- cSizeMut oldBuf

    usedCap <- length vec
    let newSize = oldSize + additional

    newBuf <- cNew newSize
    internalCopyOverN newBuf usedCap oldBuf

    writeMutVar (buf vec) newBuf

-- | \(O(n)\). Reserves capacity for at least @additional@ more elements
--   to be inserted in the given @'Vec' arr s a@. The collection
--   may reserve more space to avoid frequent reallocations.
--   After calling 'reserve', capacity will be greater than or
--   equal to @length + additional@. Does nothing if capacity
--   is already sufficient.
--
-- >>> vec <- fromFoldable @_ @PrimArray @_ @Int @_ [1]
-- >>> reserve vec 10
-- >>> assertM (>= 11) (capacity vec)
reserve :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m ()
reserve vec additional = do
  needsMore <- (\len cap -> len + additional > cap)
               <$> length vec
               <*> capacity vec
  when needsMore $ do
    oldBuf <- internal_vector vec
    oldSize <- cSizeMut oldBuf

    usedCap <- length vec
    let c = uw2d usedCap
    let newSize = ceiling (max (_GROWTH_FACTOR * c) (c + _GROWTH_FACTOR * uw2d (oldSize + additional - usedCap)))

    newBuf <- cNew newSize
    internalCopyOverN newBuf usedCap oldBuf

    writeMutVar (buf vec) newBuf

-- | \(O(n)\). Shrinks the capacity of the vector as much as possible.
--
-- >>> vec <- withCapacity @_ @Array @_ @Int 10
-- >>> extend vec [1, 2, 3]
-- >>> assertM (== 10) (capacity vec)
-- >>> shrinkToFit vec
-- >>> assertM (== 3) (capacity vec)
shrinkToFit :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m ()
shrinkToFit vec = do
  shrinkable <- (\len cap -> cap > len)
                <$> length vec
                <*> capacity vec
  when shrinkable $ do
    usedCap <- length vec
    oldBuf <- internal_vector vec
    newBuf <- cNew usedCap
    internalCopyOverN newBuf usedCap oldBuf
    writeMutVar (buf vec) newBuf

-- | \(O(n)\). Shrinks the capacity of the vector with a lower bound.
--
--   The capacity will remain at least as large as both the length and the
--   supplied value.
--
--   If the current capacity is less than the lower limit, this is a no-op.
--
-- >>> vec <- withCapacity @_ @Array @_ @Int 10
-- >>> extend vec [1, 2, 3]
-- >>> assertM (== 10) (capacity vec)
-- >>> shrinkTo vec 4
-- >>> assertM (>= 4) (capacity vec)
-- >>> shrinkTo vec 0
-- >>> assertM (>= 3) (capacity vec)
shrinkTo :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m ()
shrinkTo vec minCap = do
  cap <- capacity vec
  when (cap > minCap) $ do
    len <- length vec
    let newLen = max len minCap
    oldBuf <- internal_vector vec
    newBuf <- cNew newLen
    internalCopyOverN newBuf newLen oldBuf
    writeMutVar (buf vec) newBuf

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
unsafeRead :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m a
unsafeRead vec n = do
  v <- readMutVar (buf vec)
  cRead v n

-- | \(O(1)\). Return the element at the given position, or 'Nothing' if the index is out
--   of bounds.
--
--   The bounds are defined as [0, length).
read :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m (Maybe a)
read vec n = do
  len <- length vec
  if (n >= 0 && n < len)
    then Just <$> unsafeRead vec n
    else pure Nothing

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
unsafeWrite :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> a
  -> m ()
unsafeWrite vec n x = do
  v <- readMutVar (buf vec)
  cWrite v n x

-- | \(O(1)\). Write a value to the vector at the given position.
--
--   If the index is in bounds, this returns @'Just' ()@.
--   If the index is out of bounds, this returns 'Nothing'.
--
--   The bounds are defined as [0, length).
--   If you want to add an element past @length - 1@, use `push` or `extend`, which can
--   intelligently handle resizes.
write :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> a
  -> m (Maybe ())
write vec n x = do
  len <- length vec
  if (n >= 0 && n < len)
    then Just <$> unsafeWrite vec n x
    else pure Nothing

-- | Amortised \(O(1)\). Appends an element to the vector.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1, 2]
-- >>> push vec 3
-- >>> assertM (== [1, 2, 3]) (toList vec)
push :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> a
  -> m ()
push vec x = do
  -- reserve (not reserveExact) will take care of
  -- intelligent resizing.
  reserve vec 1

  index <- length vec
  internal_write vec index x

  internal_modify_len vec (+ 1)

{-
-- | \(O(n)\). Inserts an element at the given position, shifting all elements
--   after it to the right. Returns 'Nothing' if the given index is greater than
--   the length of the vector, otherwise returns 'Just' ().
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1, 2, 3]
-- >>> insert vec 1 4
-- >>> assertM (== [1, 4, 2, 3]) (toList vec)
-- >>> insert vec 4 5
-- >>> assertM (== [1, 4, 2, 3, 5]) (toList vec)
insert :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> a
  -> m (Maybe ())
insert vec ix x = do
  buf <- internal_vector vec
  len <- length vec
  cap <- capacity vec

  when (len == cap) $ do
    reserve vec 1

  if | ix < len -> do
         C.copyMut buf (uw2i (ix + 1)) (C.sliceMut buf (uw2i ix) (uw2i (len - ix)))
         --(uw2i (len - ix)) (C.sliceMut buf 0 (uw2i (ix + 1)))
         cWrite buf ix x
         internal_set_len vec (len + 1)
         pure (Just ())
     | ix == len -> do
         cWrite buf ix x
         internal_set_len vec (len + 1)
         pure (Just ())
     | otherwise -> do
         pure Nothing
-}

-- | \(O(1)\). Removes the last element from a vector and returns it, or 'Nothing' if it
--   is empty.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1, 2, 3]
-- >>> assertM (== Just 3) (pop vec)
-- >>> assertM (== [1, 2]) (toList vec)
pop :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m (Maybe a)
pop vec = do
  lastIndex <- length vec
  if lastIndex == 0
    then pure Nothing
    else do
      internal_modify_len vec (subtract 1)
      Just <$> unsafeRead vec (lastIndex - 1)

-- | \(O(m)\). Extend the vector with the elements of some 'Foldable' structure.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Char ['a', 'b', 'c']
-- >>> extend vec ['d', 'e', 'f']
-- >>> assertM (== "abcdef") (toList vec)
extend :: forall m arr s a t. (MonadPrim s m, Contiguous arr, Element arr a, Foldable t)
  => Vec arr s a
  -> t a
  -> m ()
extend vec xs = do
  -- TODO: inline some of this stuff
  -- to reduce redundant reads/writes
  traverse_ (push vec) xs

-- | \(O(n)\). Create a vector with the elements of some 'Foldable' structure.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1..10]
-- >>> assertM (== [1..10]) (toList vec)
fromFoldable :: forall m arr s a t. (MonadPrim s m, Contiguous arr, Element arr a, Foldable t)
  => t a
  -> m (Vec arr s a)
fromFoldable xs = do
  vec <- withCapacity (ui2w (F.length xs))
  extend vec xs
  pure vec

-- | \(O(n)\). Create a list with the elements of the vector.
--
-- >>> vec <- new @_ @PrimArray @_ @Int
-- >>> extend vec [1..5]
-- >>> toList vec
-- [1,2,3,4,5]
toList :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m [a]
toList vec = do
  buf <- internal_vector vec
  sz <- length vec
  let go :: Int -> [a] -> m [a]
      go !ix !acc =
        if ix >= 0
        then do
          x <- C.read buf ix
          go (ix - 1) (x : acc)
        else do
          pure acc
  go (uw2i (sz - 1)) []

{-
toList' :: forall arr a. (Contiguous arr, Element arr a, Show a)
  => Vec arr RealWorld a
  -> IO [a]
toList' vec = do
  buf <- internal_vector vec
  sz <- length vec
  putStrLn $ "sz=" ++ show sz
  let go :: Int -> [a] -> IO [a]
      go !ix !acc =
        if ix >= 0
        then do
          putStrLn $ "ix=" ++ show ix
          x <- C.read buf ix
          putStrLn $ "elem=" ++ show x
          go (ix - 1) (x : acc)
        else do
          pure acc
  go (uw2i (sz - 1)) []
-}

-- | \(O(n)\). Create a Primitive 'PrimitiveVector.Vector' copy of a vector.
toPrimitiveVector :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a, Prim a)
  => Vec arr s a
  -> m (PrimitiveVector.Vector a)
toPrimitiveVector vec = do
  toPrimitiveVectorWith (\_ a -> pure a) vec

-- | \(O(n)\). Create a Storable 'StorableVector.Vector' copy of a vector.
toStorableVector :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a, Storable a)
  => Vec arr s a
  -> m (StorableVector.Vector a)
toStorableVector vec = do
  toStorableVectorWith (\_ a -> pure a) vec

-- | \(O(n)\). Create a lifted 'LiftedVector.Vector' copy of a vector.
toLiftedVector :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m (LiftedVector.Vector a)
toLiftedVector vec = do
  toLiftedVectorWith (\_ a -> pure a) vec

-- | \(O(n)\). Create a Primitive 'PrimitiveVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toPrimitiveVectorWith :: forall m arr s a b. (MonadPrim s m, Contiguous arr, Element arr a, Prim b)
  => (Word -> a -> m b)
  -> Vec arr s a
  -> m (PrimitiveVector.Vector b)
toPrimitiveVectorWith f vec = do
  buf <- internal_vector vec
  len <- length vec
  PrimitiveVector.generateM (uw2i len) $ \ix -> do
    let wix = ui2w ix
    x <- cRead buf wix
    f wix x

-- | \(O(n)\). Create a Storable 'StorableVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toStorableVectorWith :: forall m arr s a b. (MonadPrim s m, Contiguous arr, Element arr a, Storable b)
  => (Word -> a -> m b)
  -> Vec arr s a
  -> m (StorableVector.Vector b)
toStorableVectorWith f vec = do
  buf <- internal_vector vec
  len <- length vec
  StorableVector.generateM (uw2i len) $ \ix -> do
    let wix = ui2w ix
    x <- cRead buf wix
    f wix x

-- | \(O(n)\). Create a lifted 'LiftedVector.Vector' copy of a vector by
-- applying a function to each element and its corresponding index.
toLiftedVectorWith :: forall m arr s a b. (MonadPrim s m, Contiguous arr, Element arr a)
  => (Word -> a -> m b)
  -> Vec arr s a
  -> m (LiftedVector.Vector b)
toLiftedVectorWith f vec = do
  buf <- internal_vector vec
  len <- length vec
  LiftedVector.generateM (uw2i len) $ \ix -> do
    let wix = ui2w ix
    x <- cRead buf wix
    f wix x

-- | \(O(n)\). Map over an array, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1..10]
-- >>> map (+ 1) vec
-- >>> assertM (== [2, 3 .. 11]) (toList vec)
map :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => (a -> a)
  -> Vec arr s a
  -> m ()
map f vec = do
  buf <- internal_vector vec
  len <- length vec
  let go !ix = when (ix < len) $ do
        a <- cRead buf ix
        cWrite buf ix (f a)
        go (ix + 1)
  go 0

-- | \(O(n)\). Map strictly over an array, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1..10]
-- >>> map' (* 2) vec
-- >>> assertM (== [2, 4 .. 20]) (toList vec)
map' :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => (a -> a)
  -> Vec arr s a
  -> m ()
map' f vec = do
  buf <- internal_vector vec
  len <- length vec
  let go !ix = when (ix < len) $ do
        a <- cRead buf ix
        let !a' = f a
        cWrite buf ix a'
        go (ix + 1)
  go 0

-- | \(O(n)\). Map over an array with a function that takes the index and its
--   corresponding element as input, modifying the elements in place.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1..10]
-- >>> imap (\ix el -> ix + 1) vec
-- >>> assertM (== zipWith (+) [0..] [1..10]) (toList vec)
imap :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => (Word -> a -> a)
  -> Vec arr s a
  -> m ()
imap f vec = do
  buf <- internal_vector vec
  len <- length vec
  let go !ix = when (ix < len) $ do
        a <- cRead buf ix
        cWrite buf ix (f ix a)
        go (ix + 1)
  go 0

-- | \(O(n)\). Map strictly over an array with a function that takes
--   the index and its corresponding element as input, modifying the
--   elements in place.
--
-- >>> vec <- fromFoldable @_ @Array @_ @Int [1..10]
-- >>> imap' (\ix el -> ix + 1) vec
-- >>> assertM (== zipWith (+) [0..] [1..10]) (toList vec)
imap' :: forall m arr s a. (MonadPrim s m, Contiguous arr, Element arr a)
  => (Word -> a -> a)
  -> Vec arr s a
  -> m ()
imap' f vec = do
  buf <- internal_vector vec
  len <- length vec
  let go !ix = when (ix < len) $ do
        a <- cRead buf ix
        let !a' = f ix a
        cWrite buf ix a'
        go (ix + 1)
  go 0

--------- INTERNALS -------------

internal_vector :: (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m (Mutable arr s a)
internal_vector = readMutVar . buf

internal_write :: (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> a
  -> m ()
internal_write vec i x = internal_vector vec >>= \v -> cWrite v i x

internal_max_capacity :: (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> m Word
internal_max_capacity vec = do
  cSizeMut =<< readMutVar (buf vec)

{-
internal_set_len :: (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> Word
  -> m ()
internal_set_len vec newLen = do
  writeMutVar (len vec) newLen
-}

internal_modify_len :: (MonadPrim s m, Contiguous arr, Element arr a)
  => Vec arr s a
  -> (Word -> Word)
  -> m ()
internal_modify_len vec f = do
  modifyMutVar' (len vec) f

ui2w :: Int -> Word
ui2w = fromIntegral

uw2i :: Word -> Int
uw2i = fromIntegral

uw2d :: Word -> Double
uw2d w = int2Double (uw2i w)

cNew :: (Contiguous arr, Element arr a, MonadPrim s m) => Word -> m (Mutable arr s a)
cNew w = C.new (uw2i w)

cRead :: (Contiguous arr, Element arr a, MonadPrim s m) => Mutable arr s a -> Word -> m a
cRead m w = C.read m (uw2i w)

cWrite :: (Contiguous arr, Element arr a, MonadPrim s m) => Mutable arr s a -> Word -> a -> m ()
cWrite m w a = C.write m (uw2i w) a

cSizeMut :: (Contiguous arr, Element arr a, MonadPrim s m) => Mutable arr s a -> m Word
cSizeMut m = ui2w <$> C.sizeMut m

internalCopyOverN :: (Contiguous arr, Element arr a, MonadPrim s m) => Mutable arr s a -> Word -> Mutable arr s a -> m ()
internalCopyOverN destination sourceLen source = do
  C.copyMut destination 0 (C.sliceMut source 0 (uw2i sourceLen))

_GROWTH_FACTOR :: Double
_GROWTH_FACTOR = 1.5
{-# inline _GROWTH_FACTOR #-}
