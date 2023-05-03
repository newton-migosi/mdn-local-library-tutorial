module LocalLibrary.Greetings.Queries (
  populateGreetingsTable,
  selectGreeting,
) where

import Database.Beam (Database, insert, insertValues, runInsert)
import Database.Beam.Postgres (Pg, Postgres)
import Database.Beam.Query (
  SqlEq ((==.)),
  SqlValable (val_),
  all_,
  filter_,
  runSelectReturningOne,
  select,
 )
import LocalLibrary.Greetings.Model (Greeting, GreetingEntity)
import Optics.Getter (view)

populateGreetingsTable ::
  forall db.
  (Database Postgres db) =>
  GreetingEntity db ->
  [Greeting] ->
  Pg ()
populateGreetingsTable table dat = runInsert $ do
  insert table $ insertValues dat

selectGreeting ::
  forall db.
  (Database Postgres db) =>
  GreetingEntity db ->
  Text ->
  Pg (Maybe Greeting)
selectGreeting table lang_code = runSelectReturningOne $ do
  select $
    filter_ (\greeting -> view #langCode greeting ==. val_ lang_code) $
      all_ table
