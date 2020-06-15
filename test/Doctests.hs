import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Dyna.hs"
  , "-XCPP"
  ]
