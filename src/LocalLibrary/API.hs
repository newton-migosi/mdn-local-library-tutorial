module LocalLibrary.API where

import Data.Pool (Pool)
import Database.Persist.SqlBackend (SqlBackend)
import Servant.Server (Server)

import LocalLibrary.Greetings.API qualified as Greetings

type API = Greetings.API

handle :: Pool SqlBackend -> Server API
handle = Greetings.handle
