module Main where

import Main.Utf8 qualified as Utf8

import LocalLibrary.Config qualified as Config

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    manageDB

manageDB :: IO ()
manageDB =
  Config.getSettings
    >>= traverse Config.createSqlConnectionPool
    >>= traverse_ (const pass)
