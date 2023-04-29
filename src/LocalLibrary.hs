{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary (
  -- * Types
  API,

  -- * Handlers
  handleLang,

  -- * Database Queries
  migrateLibraryDB,
  populateGreetingsTable,
) where

import Control.Monad.Catch (throwM)
import Data.Pool (Pool, withResource)
import Database.Beam (
  Beamable,
  Columnar,
  Database,
  DatabaseSettings,
  Table (..),
  TableEntity,
  defaultDbSettings,
  insert,
  insertValues,
  runInsert,
 )
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Simple (autoMigrate, createSchema)
import Database.Beam.Migrate.Types (CheckedDatabaseSettings)
import Database.Beam.Postgres (
  Connection,
  Postgres,
  runBeamPostgres,
 )
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Query (
  SqlEq ((==.)),
  SqlValable (val_),
  all_,
  filter_,
  runSelectReturningOne,
  select,
 )
import Optics.Core (
  A_Lens,
  LabelOptic (labelOptic),
  NoIx,
  Optic,
  lensVL,
 )
import Optics.Getter (view)
import Servant (Capture, Get, JSON, (:>))
import Servant.Server (Handler, err404)

type API = Capture "lang" Text :> Get '[JSON] Text

greetings :: [Greeting]
greetings =
  [ Greeting "en" "Hello, world!"
  , Greeting "fr" "Bonjour, monde!"
  , Greeting "es" "¡Hola, mundo!"
  , Greeting "de" "Hallo, Welt!"
  , Greeting "it" "Ciao, mondo!"
  , Greeting "pt" "Olá, mundo!"
  , Greeting "ru" "Привет, мир!"
  , Greeting "ja" "こんにちは世界!"
  , Greeting "ko" "안녕하세요 세계!"
  , Greeting "zh" "你好，世界！"
  , Greeting "ar" "مرحبا بالعالم!"
  , Greeting "he" "שלום עולם!"
  , Greeting "hi" "नमस्ते दुनिया!"
  , Greeting "bn" "ওহে বিশ্ব!"
  , Greeting "pa" "ਹੈਲੋ ਵਰਲਡ!"
  , Greeting "ta" "ஹலோ வேர்ல்ட்!"
  , Greeting "te" "హలో వరల్డ్!"
  , Greeting "ml" "ഹലോ വേൾഡ്!"
  , Greeting "th" "สวัสดีชาวโลก!"
  , Greeting "vi" "Xin chào thế giới!"
  , Greeting "id" "Halo dunia!"
  , Greeting "ms" "Hai dunia!"
  , Greeting "tr" "Merhaba dünya!"
  , Greeting "nl" "Hallo wereld!"
  , Greeting "pl" "Witaj świecie!"
  , Greeting "hu" "Helló Világ!"
  , Greeting "cs" "Ahoj světe!"
  , Greeting "sk" "Ahoj svet!"
  , Greeting "uk" "Привіт Світ!"
  , Greeting "ro" "Salut Lume!"
  , Greeting "bg" "Здравей свят!"
  , Greeting "el" "Γειά σου Κόσμε!"
  , Greeting "fa" "سلام دنیا!"
  , Greeting "ur" "ہیلو دنیا!"
  , Greeting "ne" "नमस्कार संसार!"
  , Greeting "mr" "नमस्कार जग!"
  , Greeting "gu" "હેલો વર્લ્ડ!"
  , Greeting "kn" "ಹಲೋ ವರ್ಲ್ಡ್!"
  ]

migrateLibraryDB :: Pool Connection -> IO ()
migrateLibraryDB connection = do
  withResource connection $ \conn -> do
    runBeamPostgres conn $
      autoMigrate migrationBackend checkedLocalLibraryDb

populateGreetingsTable :: Pool Connection -> IO ()
populateGreetingsTable connection = do
  withResource connection $ \conn -> do
    runBeamPostgres conn $ do
      createSchema migrationBackend checkedLocalLibraryDb
      runInsert $
        insert (view #greetings localLibraryDb) $
          insertValues greetings

handleLang :: Pool Connection -> Text -> Handler Text
handleLang connection lang = liftIO $ do
  withResource connection $ \conn -> do
    mRes <-
      runBeamPostgres conn $
        runSelectReturningOne $
          select $
            filter_ (\greeting -> view #langCode greeting ==. val_ lang) $
              all_ (view #greetings localLibraryDb)

    case mRes of
      Nothing -> throwM err404
      Just (Greeting _ greeting) -> return greeting

newtype LocalLibraryDb f = LocalLibraryDb
  { greetings :: f (TableEntity GreetingT)
  }
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

localLibraryDb :: DatabaseSettings Postgres LocalLibraryDb
localLibraryDb = defaultDbSettings

checkedLocalLibraryDb :: CheckedDatabaseSettings Postgres LocalLibraryDb
checkedLocalLibraryDb = defaultMigratableDbSettings

data GreetingT f = Greeting
  { langCode :: Columnar f Text
  , greeting :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Greeting = GreetingT Identity
type GreetingId = PrimaryKey GreetingT Identity

deriving stock instance Show Greeting
deriving stock instance Eq Greeting

deriving stock instance Show GreetingId

instance Table GreetingT where
  data PrimaryKey GreetingT f
    = GreetingId (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey ::
    forall (column :: Type -> Type).
    GreetingT column ->
    PrimaryKey GreetingT column
  primaryKey = GreetingId . view #langCode

instance
  ( lens ~ A_Lens
  , a ~ Columnar f Text
  , b ~ Columnar f Text
  ) =>
  LabelOptic "greeting" lens (GreetingT f) (GreetingT f) a b
  where
  {-# INLINE labelOptic #-}
  labelOptic :: Optic lens NoIx (GreetingT f) (GreetingT f) a b
  labelOptic =
    lensVL $ \f -> \case
      Greeting x1 x2 -> fmap (Greeting x1) (f x2)

instance
  ( lens ~ A_Lens
  , a ~ Columnar f Text
  , b ~ Columnar f Text
  ) =>
  LabelOptic "langCode" lens (GreetingT f) (GreetingT f) a b
  where
  {-# INLINE labelOptic #-}
  labelOptic :: Optic lens NoIx (GreetingT f) (GreetingT f) a b
  labelOptic =
    lensVL $ \f -> \case
      Greeting x1 x2 -> fmap (`Greeting` x2) (f x1)
