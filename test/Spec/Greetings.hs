module Spec.Greetings where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Client (
  BaseUrl (baseUrlPort),
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import Servant.Client qualified as Servant
import Test.Hspec (
  SpecWith,
  describe,
  it,
  runIO,
  shouldBe,
 )

import LocalLibrary.API qualified as LocalLibrary

spec :: SpecWith Warp.Port
spec = do
  clientEnv <- runIO $ do
    baseUrl <- parseBaseUrl "http://localhost"
    manager <- newManager defaultManagerSettings
    pure $ \port ->
      mkClientEnv manager (baseUrl {baseUrlPort = port})

  let apiClient = Servant.client (Proxy @LocalLibrary.API)

  describe "GET /{lang-code}" $ do
    it "should return greeting in english" $ \port -> do
      result <- runClientM (apiClient "en") (clientEnv port)
      result `shouldBe` Right "Hello, world!"
      pass

  pass
