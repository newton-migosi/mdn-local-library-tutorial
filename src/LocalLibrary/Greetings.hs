{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary.Greetings where

import Control.Exception.Safe (throwM)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson.Types qualified as Aeson
import Data.Pool (Pool)
import Database.Persist (
  Entity (Entity),
  SelectOpt (LimitTo),
  selectList,
  (==.),
 )
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (keyValueEntityToJSON, runSqlPool)
import Database.Persist.TH (
  MkPersistSettings (mpsGenerateLenses),
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import Relude.Extra.Lens (view)
import Servant (Capture, Get, JSON, type (:>))
import Servant.Server (Handler, Server, err404)

share
  [ mkPersist sqlSettings {mpsGenerateLenses = True}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
Greeting sql=greetings
  langCode Text
  greeting Text
  deriving Show Eq
|]

deriveJSON defaultOptions ''Greeting

type API = Capture "lang" Text :> Get '[JSON] Text

handle :: Pool SqlBackend -> Server API
handle = handleLang

queryLang :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Greeting]
queryLang lang_code =
  selectList [GreetingLangCode ==. lang_code] [LimitTo 1]

handleLang :: Pool SqlBackend -> Text -> Handler Text
handleLang pool langCode =
  liftIO $ do
    x <- runSqlPool (queryLang langCode) pool
    case x of
      [Entity _ greet] -> pure $ view greetingGreeting greet
      _ -> throwM err404

instance ToJSON (Entity Greeting) where
  toJSON :: Entity Greeting -> Aeson.Value
  toJSON = keyValueEntityToJSON
