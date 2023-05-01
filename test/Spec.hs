module Spec where

import Data.Pool (createPool)
import Database.Beam.Postgres (defaultConnectInfo)
import Database.Beam.Postgres qualified as DB
import LocalLibrary (handleLang)
import LocalLibrary qualified
import Main.Utf8 qualified as Utf8
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Client (BaseUrl (baseUrlPort), mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client qualified as Servant
import Servant.Server (serve)
import Test.Hspec (Spec, SpecWith, around, describe, hspec, it, runIO, shouldBe)
import Test.Hspec.Wai ()

main :: IO ()
main =
  Utf8.withUtf8 $ hspec spec

spec :: Spec
spec = do
  describe "LocalLibrary" $ do
    dbPool <-
      runIO $
        createPool
          (DB.connect defaultConnectInfo)
          DB.close
          1
          10
          5
    let withApp =
          Warp.testWithApplication $ do
            pure $ serve (Proxy @LocalLibrary.API) (handleLang dbPool)

    around withApp helloSpec

    pass

helloSpec :: SpecWith Warp.Port
helloSpec = do
  let helloUser = Servant.client (Proxy @LocalLibrary.API)
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

  describe "GET /{lang-code}" $ do
    it "should return greeting in english" $ \port -> do
      result <- runClientM (helloUser "en") (clientEnv port)
      result `shouldBe` Right "Hello, world!"
      pass
  pass