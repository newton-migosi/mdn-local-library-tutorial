module Spec where

import Control.Exception.Safe (throw)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server (serve)
import Test.Hspec (
  Spec,
  around,
  describe,
  hspec,
 )
import Test.Hspec.Wai ()

import LocalLibrary.API qualified as LocalLibrary
import LocalLibrary.Config qualified as Config
import Spec.Greetings qualified as Greetings

data ConfigParseError = ConfigParseError
  deriving stock (Show)
  deriving anyclass (Exception)

main :: IO ()
main =
  Utf8.withUtf8 $ do
    mDbPool <- runMaybeT $ do
      conf <- MaybeT Config.getSettings
      Config.createDbConnectionPool conf & liftIO

    dbPool <- maybe (throw ConfigParseError) pure mDbPool

    hspec $ spec dbPool

spec :: Pool Connection -> Spec
spec dbPool = do
  let app =
        serve
          (Proxy @LocalLibrary.API)
          (LocalLibrary.handle dbPool)

  let withApp =
        Warp.testWithApplication $ pure app

  describe "LocalLibrary" $ do
    around withApp Greetings.spec
    pass
