module LocalLibrary.API where

import Data.Pool (Pool)
import Database.Beam.Postgres (
  Connection,
 )
import LocalLibrary.Database.Schema (
  localLibraryDb,
 )
import LocalLibrary.Greetings.API qualified as Greetings
import Optics.Getter (view)
import Servant.Server (Server)

type API = Greetings.API

handle :: Pool Connection -> Server API
handle conn = Greetings.handleLang conn (view #greetings localLibraryDb)
