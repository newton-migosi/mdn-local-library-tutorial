module LocalLibrary.Database.Migrate where

import Data.Pool (Pool, withResource)
import Database.Beam.Migrate.Simple (autoMigrate, createSchema)
import Database.Beam.Postgres (
  Connection,
  runBeamPostgres,
 )
import Database.Beam.Postgres.Migrate (migrationBackend)
import Optics.Getter (view)

import LocalLibrary.Database.Schema (
  checkedLocalLibraryDb,
  localLibraryDb,
 )
import LocalLibrary.Greetings.Queries (populateGreetingsTable)
import LocalLibrary.Greetings.SampleData qualified as SampleData

migrateLibraryDB :: Pool Connection -> IO ()
migrateLibraryDB connection = do
  withResource connection $ \conn -> do
    runBeamPostgres conn $
      autoMigrate migrationBackend checkedLocalLibraryDb

migrateGreetingsTable :: Pool Connection -> IO ()
migrateGreetingsTable connection = do
  withResource connection $ \conn -> do
    runBeamPostgres conn $ do
      createSchema migrationBackend checkedLocalLibraryDb
      populateGreetingsTable (view #greetings localLibraryDb) SampleData.greetings
