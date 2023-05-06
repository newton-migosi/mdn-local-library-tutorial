{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary.Config where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (decodeFileStrict)
import Data.Aeson qualified as Aeson
import Data.Pool (Pool, createPool)
import Database.Persist.Postgresql (
  PostgresConf (..),
  createPostgresqlPoolWithConf,
  defaultPostgresConfHooks,
 )
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.Sqlite (
  createSqlitePoolFromInfo,
 )
import Database.Persist.Sqlite qualified as SQLite
import Database.PostgreSQL.Simple (
  ConnectInfo (..),
  Connection,
  defaultConnectInfo,
 )
import Database.PostgreSQL.Simple qualified as DB
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Http
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Optics.Getter (view)
import Optics.Optic ((%%))
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Generic (
  ParseRecord,
  Unwrapped,
  Wrapped,
  unwrapRecord,
  (:::),
  type (<?>),
 )
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

data MockAppConfig = MockAppConfig
  { poolConfig :: PoolConfig
  , warpConfig :: WarpConfig
  , dbPath :: Text
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

newtype SqliteConfig = SqliteConfig
  { database :: Text
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

data ConfigError
  = MissingEnv Text
  | DecodeFileError String
  deriving stock (Show)
  deriving anyclass (Exception)

liftMaybe :: forall f t l r. Applicative f => (t -> l) -> t -> Maybe r -> f (Either l r)
liftMaybe f e x = pure $ maybeToRight (f e) x

getSettingsEnv :: IO (Maybe AppConfig)
getSettingsEnv = runMaybeT $ do
  poolConfig <-
    MaybeT (lookupEnv "POOL_CONFIG")
      >>= MaybeT . decodeFileStrict
  warpConfig <-
    MaybeT (lookupEnv "WARP_CONFIG")
      >>= MaybeT . decodeFileStrict
  postgresConfig <-
    MaybeT (lookupEnv "POSTGRES_CONFIG")
      >>= MaybeT . decodeFileStrict
  pure $
    AppConfig poolConfig warpConfig postgresConfig

-- read sqlite db file from env
getMockSettingsEnv :: IO (Either ConfigError MockAppConfig)
getMockSettingsEnv = runExceptT $ do
  poolConfig <-
    ExceptT (lookupEnv "POOL_CONFIG" >>= liftMaybe MissingEnv "POOL_CONFIG")
      >>= ExceptT . (\x -> decodeFileStrict x >>= liftMaybe DecodeFileError x)
  warpConfig <-
    ExceptT (lookupEnv "WARP_CONFIG" >>= liftMaybe MissingEnv "WARP_CONFIG")
      >>= ExceptT . (\x -> decodeFileStrict x >>= liftMaybe DecodeFileError x)
  sqliteConfig <-
    ExceptT (lookupEnv "SQLITE_DB" >>= liftMaybe MissingEnv "SQLITE_DB") <&> toText
  pure $
    MockAppConfig poolConfig warpConfig sqliteConfig

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

createSqlConnectionPool :: AppConfig -> IO (Pool SqlBackend)
createSqlConnectionPool conf = runStderrLoggingT $ do
  createPostgresqlPoolWithConf (mkPostgresConf conf) defaultPostgresConfHooks

createSqlLiteConnectionPool :: MockAppConfig -> IO (Pool SqlBackend)
createSqlLiteConnectionPool conf = runStderrLoggingT $ do
  let connectInfo = SQLite.mkSqliteConnectionInfo (view #dbPath conf)
  createSqlitePoolFromInfo connectInfo (view (#poolConfig %% #maxResources) conf)

mkPostgresConf :: AppConfig -> PostgresConf
mkPostgresConf conf =
  PostgresConf
    { pgConnStr =
        DB.postgreSQLConnectionString $
          mkDatabaseConnectInfo $
            view #postgresConfig conf
    , pgPoolSize = view (#poolConfig %% #maxResources) conf
    , pgPoolIdleTimeout = view (#poolConfig %% #idleTime) conf
    , pgPoolStripes = view (#poolConfig %% #numStripes) conf
    }

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
makeFieldLabelsNoPrefix ''MockAppConfig
makeFieldLabelsNoPrefix ''SqliteConfig
makeFieldLabelsNoPrefix ''PostgresConfig
makeFieldLabelsNoPrefix ''WarpConfig
makeFieldLabelsNoPrefix ''PoolConfig
