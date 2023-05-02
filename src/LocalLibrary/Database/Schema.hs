{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary.Database.Schema where

import Database.Beam (
  Database,
  DatabaseSettings,
  TableEntity,
  defaultDbSettings,
 )
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Types (CheckedDatabaseSettings)
import Database.Beam.Postgres (
  Postgres,
 )

import LocalLibrary.Greetings.Model (GreetingT)
import Optics.TH (makeFieldLabelsNoPrefix)

newtype LocalLibraryDb f = LocalLibraryDb
  { greetings :: f (TableEntity GreetingT)
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

localLibraryDb :: DatabaseSettings Postgres LocalLibraryDb
localLibraryDb = defaultDbSettings

checkedLocalLibraryDb :: CheckedDatabaseSettings Postgres LocalLibraryDb
checkedLocalLibraryDb = defaultMigratableDbSettings

makeFieldLabelsNoPrefix ''LocalLibraryDb
