{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LocalLibrary.Greetings.Model where

import Database.Beam (
  Beamable,
  Columnar,
  Table (..),
  TableEntity,
 )
import Database.Beam.Postgres (
  Postgres,
 )
import Database.Beam.Schema.Tables (DatabaseEntity)
import Optics.Getter (view)
import Optics.TH (makeFieldLabelsNoPrefix)

data GreetingT f = Greeting
  { langCode :: Columnar f Text
  , greeting :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Greeting = GreetingT Identity
type GreetingId = PrimaryKey GreetingT Identity

type GreetingEntity a = DatabaseEntity Postgres a (TableEntity GreetingT)

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

makeFieldLabelsNoPrefix ''GreetingT
