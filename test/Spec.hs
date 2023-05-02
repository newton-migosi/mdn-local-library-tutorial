module Spec where

import Data.Pool (createPool)
import Database.Beam.Postgres (defaultConnectInfo)
import Database.Beam.Postgres qualified as DB
import LocalLibrary.API qualified as LocalLibrary
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server (serve)
import Spec.Greetings qualified as Greetings
import Test.Hspec (
  Spec,
  around,
  describe,
  hspec,
  runIO,
 )
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
            pure $ serve (Proxy @LocalLibrary.API) (LocalLibrary.handle dbPool)

    around withApp Greetings.spec

    pass
