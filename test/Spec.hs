module Spec where

import Control.Exception.Safe (throw)
import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server (serve)
import Spec.Greetings qualified as Greetings
import Test.Hspec (
  Spec,
  around,
  describe,
  hspec,
 )
import Test.Hspec.Wai ()

import LocalLibrary.API qualified as LocalLibrary
import LocalLibrary.Config qualified as Config

data ConfigParseError = ConfigParseError
  deriving stock (Show)
  deriving anyclass (Exception)

runTests :: IO ()
runTests = do
  mDbPool <- runMaybeT $ do
    conf <- MaybeT Config.getSettingsEnv
    Config.createSqlConnectionPool conf & liftIO

  dbPool <- maybe (throw ConfigParseError) pure mDbPool

  hspec $ spec dbPool

spec :: Pool SqlBackend -> Spec
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
