{-# language TypeApplications #-}

module Main (main) where

import Dyna (Vec)
import qualified Dyna as Vec
import Control.Monad
import System.Exit
import qualified Dyna.Compact as C

main :: IO ()
main = do
  c <- C.new
  let n :: [Int]
      n = [0..99]
  forM_ n $ \i -> C.write c i i
  forM_ n $ \i -> do
    k <- C.read c i
    print k



{-
  v <- Vec.fromFoldable @_ @_ @Int [0, 2, 4, 6]
  two <- Vec.read v 1
  Vec.write v 1 7
  seven <- Vec.read v 1
  when (two + seven /= 9) exitFailure
-}
