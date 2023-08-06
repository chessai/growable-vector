{-# language ImportQualifiedPost, TypeApplications #-}

{-# options_ghc -Wunused-top-binds #-}

module Main (main) where

import Data.Primitive.Contiguous (Array, SmallArray, PrimArray)
import Dyna
import Control.Monad
import Prelude hiding (length)
import Data.Vector.Primitive qualified as PrimitiveVector
import Data.Vector qualified as LiftedVector

_assert :: (Show a) => (a -> Bool) -> a -> IO ()
_assert p a = assertM p (pure a)

assertM :: (Show a) => (a -> Bool) -> IO a -> IO ()
assertM p ma = do
  a <- ma
  when (not (p a)) $ do
    error ("error: " ++ show a)

main :: IO ()
main = do
  t_new
  t_withCapacity
  t_length
  t_capacity
  t_reserveExact
  t_reserve
  t_shrinkToFit
  t_shrinkTo

  --toVector
  t_new

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

_toVector :: IO ()
_toVector = do
  vec <- fromFoldable @_ @Array @_ @Int [1..1000]

  primVec <- toPrimitiveVector vec
  assertM (== [1..1000]) (pure (PrimitiveVector.toList primVec))

  liftedVec <- toLiftedVector vec
  assertM (== [1..1000]) (pure (LiftedVector.toList liftedVec))
