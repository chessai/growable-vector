{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}

module Dyna
  ( GrowableVector
  , new
  , size
  , read
  , push
  , modify
  , swap
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive
import Data.Vector.Generic.Mutable (MVector)
import GHC.Exts
import Prelude hiding (read)

import qualified Data.Vector.Generic.Mutable as Vector

data GrowableVector v s a = GrowableVector
  { sizeVar :: {-# unpack #-} !(MutVar s Int)
  , vectorVar :: {-# unpack #-} !(MutVar s (v s a))
  }

size :: (MVector v a, PrimMonad m)
  => GrowableVector v (PrimState m) a
  -> m Int
size = readMutVar . sizeVar

new :: (MVector v a, PrimMonad m)
  => Int
  -> m (GrowableVector v (PrimState m) a)
new sz = GrowableVector
  <$> newMutVar 0
  <*> (newMutVar =<< Vector.new sz)

read :: (MVector v a, PrimMonad m)
  => GrowableVector v (PrimState m) a
  -> Int
  -> m a
read g n = do
  vec <- readMutVar (vectorVar g)
  Vector.unsafeRead vec n

push :: (MVector v a, PrimMonad m)
  => GrowableVector v (PrimState m) a
  -> a
  -> m ()
push g x = do
  needsResizing <- atMaxCapacity
  when needsResizing resize

  index <- readMutVar (sizeVar g)
  readMutVar (vectorVar g) >>= \v -> Vector.write v index x

  modifyMutVar' (sizeVar g) (+ 1)

  where
    resize = do
      v <- readMutVar (vectorVar g)
      v' <- Vector.unsafeGrow v (Vector.length v)
      writeMutVar (vectorVar g) v'
    atMaxCapacity = do
      sz <- readMutVar (sizeVar g)
      len <- Vector.length <$> readMutVar (vectorVar g)
      pure (sz + 1 >= len)

modify :: (MVector v a, PrimMonad m)
  => GrowableVector v (PrimState m) a
  -> (a -> a)
  -> Int
  -> m ()
modify g f ix = readMutVar (vectorVar g) >>= \v -> Vector.modify v f ix

swap :: (MVector v a, PrimMonad m)
  => GrowableVector v (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap g x y = readMutVar (vectorVar g) >>= \v -> Vector.swap v x y
