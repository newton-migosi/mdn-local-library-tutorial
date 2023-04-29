module Main (main) where

import Data.Pool (createPool)
import Database.Beam.Postgres (defaultConnectInfo)
import Database.Beam.Postgres qualified as DB
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (Status)
import Network.HTTP.Types qualified as Http
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Options.Generic (getRecord)
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  hsep,
  layoutPretty,
  parens,
  viaShow,
 )
import Prettyprinter.Render.Text (renderStrict)
import Servant qualified
import Text.Printf (printf)

import LocalLibrary (
  API,
  handleLang,
  migrateLibraryDB,
  populateGreetingsTable,
 )
import LocalLibrary.Config (
  InitServer (RunWarpServer, WriteMigrationScript),
 )

{- |
 Main entry point.

 The `, run` script will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    runServer

runServer :: IO ()
runServer = do
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setLogger logger
          & Warp.setTimeout timeout

      timeout = 30
      port = 8081

      connectInfo = defaultConnectInfo

  arg <- getRecord ""

  dbPool <-
    createPool
      (DB.connect connectInfo)
      DB.close
      1
      10
      5

  printf "Starting server on port 8081\n"

  case arg of
    RunWarpServer -> do
      Servant.serve (Proxy @API) (handleLang dbPool)
        & Warp.runSettings settings
    WriteMigrationScript -> do
      migrateLibraryDB dbPool
      populateGreetingsTable dbPool

logger :: Request -> Status -> Maybe Integer -> IO ()
logger req status _maybeFileSize =
  putTextLn . renderStrict . layoutPretty defaultLayoutOptions $
    hsep
      [ "[info]"
      , viaShow $ Wai.requestMethod req
      , viaShow $ Wai.rawPathInfo req
      , parens $ pretty $ Http.statusCode status
      ]
