module Spec where

import Control.Exception.Safe (throw)
import Data.Pool (Pool)
import Database.Persist.Postgresql (runMigration, runSqlPool)
import Database.Persist.SqlBackend (SqlBackend)
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
import LocalLibrary.Greetings.API (migrateAll, populateGreetingsTable)
import LocalLibrary.Greetings.SampleData qualified as SampleData
import Optics.Setter (set)

data ConfigParseError = ConfigParseError
  deriving stock (Show)
  deriving anyclass (Exception)

runTests :: IO ()
runTests = do
  mDbPool <- runExceptT $ do
    conf <- ExceptT Config.getMockSettingsEnv'
    let conf' = conf & set #dbPath "test.db"
    Config.createSqlLiteConnectionPool conf' & liftIO

  dbPool <- either throw pure mDbPool

  let migrateGreetingsTable = do
        runMigration migrateAll
        populateGreetingsTable SampleData.greetings

  liftIO $
    runSqlPool migrateGreetingsTable dbPool

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
