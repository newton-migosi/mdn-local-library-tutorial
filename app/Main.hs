module Main (main) where

import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp qualified as Warp
import Optics.Getter (view)
import Servant (serve)

import LocalLibrary.API (
  API,
  handle,
 )
import LocalLibrary.Config qualified as Config

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
runServer = void $ runMaybeT $ do
  conf <- MaybeT Config.getSettings

  let runSettings =
        Warp.runSettings
          . Warp.setLogger Config.customLogger
          . Config.mkWarpSettings

  dbPool <- Config.createDbConnectionPool conf & liftIO

  Servant.serve (Proxy @API) (handle dbPool)
    & runSettings (view #warpConfig conf)
    & liftIO
