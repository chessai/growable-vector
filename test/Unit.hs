{-# language TypeApplications #-}

module Main (main) where

import Dyna (Vec)
import qualified Dyna as Vec
import Control.Monad
import System.Exit

main :: IO ()
main = do
  v <- Vec.fromFoldable @_ @_ @Int [0, 2, 4, 6]
  two <- Vec.read v 1
  Vec.write v 1 7
  seven <- Vec.read v 1
  when (two + seven /= 9) exitFailure
