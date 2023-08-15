{-# language ImportQualifiedPost, LambdaCase, TypeApplications #-}

{-# options_ghc -Wunused-top-binds #-}

module Main (main) where

import Data.Primitive.Contiguous (Array, SmallArray, PrimArray, Contiguous, Element)
import GrowableVector
import Control.Exception (try, ErrorCall(..))
import Control.Monad
import GHC.Exts (RealWorld)
import Prelude hiding (length, read, map)
import Data.Vector qualified as LiftedVector
import Data.Vector.Primitive qualified as PrimitiveVector
import Data.Vector.Storable qualified as StorableVector

_assert :: (Show a) => (a -> Bool) -> a -> IO ()
_assert p a = assertM p (pure a)

assertM :: (Show a) => (a -> Bool) -> IO a -> IO ()
assertM p ma = do
  a <- ma
  when (not (p a)) $ do
    error ("error: " ++ show a)

main :: IO ()
main = do
  putStrLn "Running..."

  t_new
  t_withCapacity
  t_length
  t_capacity
  t_reserveExact
  t_reserve
  t_shrinkToFit
  t_shrinkTo
  t_read
  t_write
  t_push
  t_pop
  --t_insert
  t_extend
  t_fromFoldable
  t_toList
  t_toVector
  t_toVectorWith
  t_map
  t_map'
  t_imap
  t_imap'

t_new :: IO ()
t_new = do
  vec <- new @_ @Array @_ @Int
  assertM (== 0) (length vec)
  assertM (== 0) (capacity vec)

t_withCapacity :: IO ()
t_withCapacity = do
  vec <- withCapacity @_ @SmallArray @_ @Int 10
  assertM (== 0) (length vec)
  assertM (== 10) (capacity vec)

  forM_ [1..10] $ \i -> push vec i
  assertM (== 10) (length vec)
  assertM (== 10) (capacity vec)

  push vec 11
  assertM (== 11) (length vec)
  assertM (>= 11) (capacity vec)

t_length :: IO ()
t_length = do
  vec <- new @_ @Array @_ @Int
  assertM (== 0) (length vec)

  forM_ [1..10] $ \i -> push vec i
  assertM (== 10) (length vec)

t_capacity :: IO ()
t_capacity = do
  vec <- withCapacity @_ @PrimArray @_ @Word 10
  assertM (== 10) (capacity vec)

  let x = 42
  push vec x
  assertM (== 10) (capacity vec)

  forM_ [x + 1 .. x + 9] $ \i -> push vec i
  assertM (== 10) (capacity vec)

  push vec (x + 10)
  assertM (> 10) (capacity vec)

t_reserveExact :: IO ()
t_reserveExact = do
  vec <- fromFoldable @_ @Array @_ @Int @_ [1]
  reserveExact vec 10
  assertM (>= 11) (capacity vec)

t_reserve :: IO ()
t_reserve = do
  vec <- fromFoldable @_ @PrimArray @_ @Int @_ [1]

  reserve vec 10
  assertM (>= 11) (capacity vec)

t_shrinkToFit :: IO ()
t_shrinkToFit = do
  vec <- withCapacity @_ @Array @_ @Int 10
  extend vec [1, 2, 3]
  assertM (== 10) (capacity vec)
  shrinkToFit vec
  assertM (== 3) (capacity vec)

t_shrinkTo :: IO ()
t_shrinkTo = do
  vec <- withCapacity @_ @Array @_ @Int 10
  extend vec [1, 2, 3]
  assertM (== 10) (capacity vec)

  shrinkTo vec 4
  assertM (== 4) (capacity vec)

  shrinkTo vec 0
  assertM (== 3) (capacity vec)

t_read :: IO ()
t_read = do
  vec <- withCapacity @_ @Array @_ @Int 10
  extend vec [1, 2, 3]

  assertM (== Just 1) (read vec 0)
  assertM (== Just 2) (read vec 1)
  assertM (== Just 3) (read vec 2)
  assertM (== Nothing) (read vec 3)

t_write :: IO ()
t_write = do
  vec <- withCapacity @_ @Array @_ @Int 10
  extend vec [1, 2, 3]

  assertM (== Just ()) (write vec 0 7)
  assertM (== Just ()) (write vec 1 8)
  assertM (== Just ()) (write vec 2 9)
  assertM (== Nothing) (write vec 3 10)

t_push :: IO ()
t_push = do
  vec <- fromFoldable @_ @Array @_ @Int [1, 2]
  push vec 3
  assertM (== [1, 2, 3]) (toList vec)

t_pop :: IO ()
t_pop = do
  vec <- fromFoldable @_ @Array @_ @Int [1, 2, 3]
  assertM (== Just 3) (pop vec)
  assertM (== [1, 2]) (toList vec)

{-
pStuff :: (Contiguous arr, Element arr a) => Vec arr RealWorld a -> IO ()
pStuff vec = do
  len <- length vec
  cap <- capacity vec
  putStrLn $ "len=" ++ show len ++ ",cap=" ++ show cap

catchBottom :: IO a -> IO ()
catchBottom io = do
  try @ErrorCall io >>= \case
    Left (ErrorCallWithLocation err loc) -> do
      putStrLn $ err
    Right _a -> pure () --error (msg ++ ": should have thrown exception on bottom")
-}

{-
t_insert :: IO ()
t_insert = do
  vec <- fromFoldable @_ @Array @_ @Int [1, 2, 3]
  pStuff vec
  assertM (== Just ()) (insert vec 1 4)
  assertM (== [1, 4, 2, 3]) (toList' vec)
  pStuff vec
  assertM (== Just ()) (insert vec 4 5)
  assertM (== [1, 4, 2, 3, 5]) (toList vec)
-}

t_extend :: IO ()
t_extend = do
  vec <- fromFoldable @_ @Array @_ @Char ['a', 'b', 'c']
  extend vec ['d', 'e', 'f']
  assertM (== "abcdef") (toList vec)

t_fromFoldable :: IO ()
t_fromFoldable = do
  vecFromList <- fromFoldable @_ @Array @_ @Int [1..10]
  assertM (== [1..10]) (toList vecFromList)

  vecFromVector <- fromFoldable @_ @Array @_ @Int (LiftedVector.fromList [1..10])
  assertM (== [1..10]) (toList vecFromVector)

t_toList :: IO ()
t_toList = do
  vec <- new @_ @PrimArray @_ @Int
  extend vec [1..5]
  assertM (== [1..5]) (toList vec)

t_toVector :: IO ()
t_toVector = do
  vec <- fromFoldable @_ @Array @_ @Int [1..1000]

  liftedVec <- toLiftedVector vec
  assertM (== [1..1000]) (pure (LiftedVector.toList liftedVec))

  primVec <- toPrimitiveVector vec
  assertM (== [1..1000]) (pure (PrimitiveVector.toList primVec))

  storableVec <- toStorableVector vec
  assertM (== [1..1000]) (pure (StorableVector.toList storableVec))

t_toVectorWith :: IO ()
t_toVectorWith = do
  vec <- fromFoldable @_ @Array @_ @Word [1..1000]

  let f = \ix el -> pure (ix + el)

  referenceList <- forM (zip [0..] [1..1000]) $ \(ix, el) -> f ix el

  liftedVec <- toLiftedVectorWith f vec
  assertM (== referenceList) (pure (LiftedVector.toList liftedVec))

  primVec <- toPrimitiveVectorWith f vec
  assertM (== referenceList) (pure (PrimitiveVector.toList primVec))

  storableVec <- toStorableVectorWith f vec
  assertM (== referenceList) (pure (StorableVector.toList storableVec))

t_map :: IO ()
t_map = do
  vec <- fromFoldable @_ @Array @_ @Word [1..10]
  extend vec [undefined]
  map (+ 1) vec
  x <- pop vec
  assertM (== [2..11]) (toList vec)

t_map' :: IO ()
t_map' = do
  vec <- fromFoldable @_ @Array @_ @Word [1..10]
  extend vec [undefined]
  expectError "t_map'" (map' (+ 1) vec)
  x <- pop vec
  assertM (== [2..11]) (toList vec)

t_imap :: IO ()
t_imap = do
  vec <- fromFoldable @_ @Array @_ @Word [1..10]
  extend vec [undefined]
  imap (\ix el -> ix + el) vec
  x <- pop vec
  assertM (== zipWith (+) [0..9] [1..10]) (toList vec)

t_imap' :: IO ()
t_imap' = do
  vec <- fromFoldable @_ @Array @_ @Word [1..10]
  extend vec [undefined]
  expectError "t_imap'" (imap' (\ix el -> ix + el) vec)
  x <- pop vec
  assertM (== zipWith (+) [0..9] [1..10]) (toList vec)

expectError :: String -> IO a -> IO ()
expectError msg x = do
  try @ErrorCall x >>= \case
    Left _ -> pure ()
    Right _ -> error (msg ++ ": should have thrown exception on bottom")
