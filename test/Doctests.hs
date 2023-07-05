module Main (main) where

import Test.DocTest (mainFromCabalWithConfig)
import Test.DocTest.Internal.Options (Config(..), defaultConfig)
import System.Environment (getArgs)

main :: IO ()
main = do
  mainFromCabalWithConfig "dyna" $ defaultConfig
    { cfgThreads = Just 1
    }
