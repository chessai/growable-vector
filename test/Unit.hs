{-# language TypeApplications #-}

module Main (main) where

import Data.Primitive.Contiguous (Array)
import Dyna.General
import Control.Monad
import Prelude hiding (length)

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
