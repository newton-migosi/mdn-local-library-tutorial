module Main where

import Main.Utf8 qualified as Utf8

import LocalLibrary.Config (createDbConnectionPool, getSettings)
import LocalLibrary.Database.Migrate (migrateLibraryDB)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    manageDB

manageDB :: IO ()
manageDB =
  getSettings
    >>= traverse createDbConnectionPool
    >>= traverse_ migrateLibraryDB
