module LocalLibrary.Greetings.API where

import Control.Monad.Catch (throwM)
import Data.Pool (Pool, withResource)
import Database.Beam (
  Database,
 )
import Database.Beam.Postgres (
  Connection,
  Postgres,
  runBeamPostgres,
 )
import LocalLibrary.Greetings.Model (
  GreetingEntity,
  GreetingT (Greeting),
 )
import LocalLibrary.Greetings.Queries (selectGreeting)
import Servant (Capture, Get, JSON, (:>))
import Servant.Server (Handler, err404)

type API = Capture "lang" Text :> Get '[JSON] Text

handleLang ::
  forall db.
  (Database Postgres db) =>
  Pool Connection ->
  GreetingEntity db ->
  Text ->
  Handler Text
handleLang connection table lang = liftIO $ do
  withResource connection $ \conn -> do
    mRes <-
      runBeamPostgres conn $ selectGreeting table lang

    case mRes of
      Nothing -> throwM err404
      Just (Greeting _ greeting) -> return greeting
