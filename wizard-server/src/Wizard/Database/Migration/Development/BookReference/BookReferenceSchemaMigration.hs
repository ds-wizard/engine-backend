module Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/BookReference) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/BookReference) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/BookReference) drop tables"
  let sql = "DROP TABLE IF EXISTS book_reference;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/BookReference) create table"
  let sql =
        "CREATE TABLE book_reference \
        \( \
        \    short_uuid   varchar     NOT NULL, \
        \    book_chapter varchar     NOT NULL, \
        \    content      varchar     NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT book_reference_pk PRIMARY KEY (short_uuid) \
        \); \
        \ \
        \CREATE UNIQUE INDEX book_reference_short_uuid_uindex ON book_reference (short_uuid);"
  let action conn = execute_ conn sql
  runDB action
