module Main where

import Main.Utf8 qualified as Utf8

-- import Spec (runTests)

main :: IO ()
main = Utf8.withUtf8 $ do
  -- runTests
  pass
