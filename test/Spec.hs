module Spec where

import Main.Utf8 qualified as Utf8
import Test.Hspec (Spec, describe, hspec, it, pendingWith)
import Test.Hspec.Wai ()

main :: IO ()
main =
  Utf8.withUtf8 $ hspec spec

spec :: Spec
spec = do
  describe "LocalLibrary" $ do
    it "should have tests" $ do
      pendingWith "TODO: Add tests"