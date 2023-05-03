{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary.Config where

import Data.Aeson (decodeFileStrict)
import Data.Aeson qualified as Aeson
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, defaultConnectInfo)
import Database.PostgreSQL.Simple qualified as DB
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Http
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Optics.Getter (view)
import Optics.Optic ((%%))
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Generic (ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>))
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  hsep,
  layoutPretty,
  parens,
  viaShow,
 )
import Prettyprinter.Render.Text (renderStrict)

data AppConfig = AppConfig
  { poolConfig :: PoolConfig
  , warpConfig :: WarpConfig
  , postgresConfig :: PostgresConfig
  }
  deriving stock (Show, Generic)

data PoolConfig = PoolConfig
  { numStripes :: Int
  , idleTime :: Integer
  , maxResources :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data WarpConfig = WarpConfig
  { port :: Warp.Port
  , timeout :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data PostgresConfig = PostgresConfig
  { server :: String
  , database :: String
  , user :: String
  , password :: String
  , port :: Word16
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data ConfigFilePaths w = ConfigFilePaths
  { poolConfig :: w ::: FilePath <?> "Path to configuration for managing connections to PostgreSQL"
  , warpConfig :: w ::: FilePath <?> "Path to configuration for setting up warp server"
  , postgresConfig :: w ::: FilePath <?> "Path to configuration for connecting to PostgreSQL"
  }
  deriving stock (Generic)

instance ParseRecord (ConfigFilePaths Wrapped)
deriving stock instance Show (ConfigFilePaths Unwrapped)

getSettings :: IO (Maybe AppConfig)
getSettings = do
  filePaths <- unwrapRecord @_ @ConfigFilePaths "Local Library server"
  poolConfig <- decodeFileStrict (view #poolConfig filePaths)
  warpConfig <- decodeFileStrict (view #warpConfig filePaths)
  postgresConfig <- decodeFileStrict (view #postgresConfig filePaths)
  pure $
    AppConfig <$> poolConfig <*> warpConfig <*> postgresConfig

customLogger :: Request -> Status -> Maybe Integer -> IO ()
customLogger req status _maybeFileSize =
  putTextLn . renderStrict . layoutPretty defaultLayoutOptions $
    hsep
      [ "[info]"
      , viaShow $ Wai.requestMethod req
      , viaShow $ Wai.rawPathInfo req
      , parens $ pretty $ Http.statusCode status
      ]

createDbConnectionPool :: AppConfig -> IO (Pool Connection)
createDbConnectionPool conf = do
  createPool
    (DB.connect $ mkDatabaseConnectInfo $ view #postgresConfig conf)
    DB.close
    (view (#poolConfig %% #numStripes) conf)
    (fromInteger $ view (#poolConfig %% #idleTime) conf)
    (view (#poolConfig %% #maxResources) conf)

mkDatabaseConnectInfo :: PostgresConfig -> ConnectInfo
mkDatabaseConnectInfo postgresConfig =
  defaultConnectInfo
    { connectHost = view #server postgresConfig
    , connectPort = view #port postgresConfig
    , connectUser = view #user postgresConfig
    , connectPassword = view #password postgresConfig
    , connectDatabase = view #database postgresConfig
    }

mkWarpSettings :: WarpConfig -> Warp.Settings
mkWarpSettings warpConfig =
  Warp.defaultSettings
    & Warp.setPort (view #port warpConfig)
    & Warp.setTimeout (view #timeout warpConfig)

makeFieldLabelsNoPrefix ''AppConfig
makeFieldLabelsNoPrefix ''PostgresConfig
makeFieldLabelsNoPrefix ''WarpConfig
makeFieldLabelsNoPrefix ''PoolConfig
