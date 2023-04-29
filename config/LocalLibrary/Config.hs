module LocalLibrary.Config (InitServer (..)) where

import Network.Wai.Handler.Warp (Port)
import Options.Generic (ParseRecord, type (<?>))

data WarpConfig = WarpConfig
  { port :: Port <?> "Port to listen on"
  , timeout :: Int <?> "Timeout in seconds"
  }
  deriving stock (Generic, Show)
  deriving anyclass (ParseRecord)

data InitServer
  = WriteMigrationScript
  | RunWarpServer
  deriving stock (Generic, Show)
  deriving anyclass (ParseRecord)
