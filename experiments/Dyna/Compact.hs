{-# language
    BangPatterns
  , CPP
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , TypeFamilies
  , UnboxedTuples
#-}

module Dyna.Compact where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Void (Void)
import Data.Foldable (traverse_)
import Data.Primitive
import Data.Primitive.MVar
import GHC.Exts hiding (compactAdd#, compactNew#)
import GHC.Stable (StablePtr(..), freeStablePtr)
import Prelude hiding (length, read)
import qualified Data.Foldable as Foldable
import qualified GHC.Exts as Exts

--data Primer a = Primer (StablePtr# a)

--newtype Primer a = Primer Any

--instance Prim (Primer a) where

--data Primer s a = Primer Int# (MutableByteArray# s)

--newPrimer :: MonadPrim s m => a -> m (Primer s a)
--newPrimer a = primitive $ \s0# -> case newByteArray# 0# s0# of
--  (# s1#, ba #) -> (# s1#, Primer (addr2Int# (unsafeCoerce# ba)) ba #)

class (PrimMonad m, s ~ PrimState m) => MonadPrim s m
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

-- version of withMVar that is not exception-safe
withMVar :: MonadPrim s m => MVar s a -> (a -> m b) -> m b
withMVar m f = do
  a <- takeMVar m
  b <- f a
  putMVar m a
  pure b

data C s a = C
  { __region :: Compact#
  , __lock :: {-# unpack #-} !(MVar s ())
  , __lol :: {-# unpack #-} !(MutablePrimArray s (Ptr Void))
  }

new :: (MonadPrim s m) => m (C s a)
new = do
  lock <- newMVar ()
  arr <- newPrimArray 100
  primitive $ \s0 -> case compactNew# 31268## s0 of
    (# s1, compact# #) -> (# s1, C compact# lock arr #)

compactNew# :: Word# -> State# s -> (# State# s, Compact# #)
compactNew# = unsafeCoerce# Exts.compactNew#

compactAdd# :: Compact# -> a -> State# s -> (# State# s, a #)
compactAdd# = unsafeCoerce# Exts.compactAdd#

write :: (MonadPrim s m) => C s a -> Int -> a -> m ()
write (C region lock arr) i x = do
  c <- do
    withMVar lock
    $ \_ -> primitive
    $ \s0 -> compactAdd# region x s0
  p <- primitive $ \s0 ->
    let !(# s1, a #) = anyToAddr# c (unsafeCoerce# s0)
    in (# unsafeCoerce# s1, Ptr a #)
  writePrimArray arr i p

read :: (MonadPrim s m) => C s a -> Int -> m a
read (C _ _ arr) i = do
  Ptr a <- readPrimArray arr i
  let !(# x #) = addrToAny# a
  pure x

{-
data Vec s a = Vec
  { len :: {-# unpack #-} !(MutVar s Int)
  , buf :: {-# unpack #-} !(MutVar s (MutablePrimArray s a))
  }

new :: forall m s a. (MonadPrim s m, Prim a)
  => m (Vec s a)
new = withCapacity 0

withCapacity :: forall m s a. (MonadPrim s m, Prim a)
  => Int
  -> m (Vec s a)
withCapacity sz = Vec
  <$> newMutVar 0
  <*> (newMutVar =<< newPrimArray sz)
-}

