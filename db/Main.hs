module Main where

import Main.Utf8 qualified as Utf8

import Database.Persist.Postgresql (runMigration, runSqlPool)

import LocalLibrary.Config qualified as Config
import LocalLibrary.Greetings.API (migrateAll, populateGreetingsTable)
import LocalLibrary.Greetings.SampleData qualified as SampleData

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    manageDB

manageDB :: IO ()
manageDB = void $ runMaybeT $ do
  conf <- MaybeT Config.getMockSettingsEnv
  dbPool <- Config.createSqlLiteConnectionPool conf & liftIO
  let migrateGreetingsTable = do
        runMigration migrateAll
        populateGreetingsTable SampleData.greetings
  liftIO $
    runSqlPool migrateGreetingsTable dbPool
  pass
