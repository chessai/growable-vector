{-# language ImportQualifiedPost, TypeApplications #-}

module Main (main) where

import Data.Primitive.Contiguous (Array)
import Dyna
import Control.Monad
import Prelude hiding (length)
import Data.Vector.Primitive qualified as PrimitiveVector
import Data.Vector qualified as LiftedVector

assertM :: (Show a) => (a -> Bool) -> IO a -> IO ()
assertM p ma = do
  a <- ma
  when (not (p a)) $ do
    error ("error: " ++ show a)

main :: IO ()
main = do
  vec <- withCapacity @_ @Array @_ @Int 10
  assertM (== 0) $ length vec
  assertM (== 10) $ capacity vec

  forM_ [1..10] $ \i -> push vec i
  assertM (== 10) (length vec)
  assertM (== 10) (capacity vec)

  push vec 11
  assertM (== 11) (length vec)
  assertM (>= 11) (capacity vec)

  assertM (== [1..11]) (toList vec)

  toVector

toVector :: IO ()
toVector = do
  vec <- fromFoldable @_ @Array @_ @Int [1..1000]

  primVec <- toPrimitiveVector vec
  assertM (== [1..1000]) (pure (PrimitiveVector.toList primVec))

  liftedVec <- toLiftedVector vec
  assertM (== [1..1000]) (pure (LiftedVector.toList liftedVec))
