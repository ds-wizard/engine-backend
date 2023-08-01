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
  let sql = "drop table if exists book_reference;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/BookReference) create table"
  let sql =
        " create table book_reference \
        \ ( \
        \   short_uuid varchar not null, \
        \   book_chapter varchar not null, \
        \   content varchar not null, \
        \   created_at timestamptz not null, \
        \   updated_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index book_reference_short_uuid_uindex \
        \    on book_reference (short_uuid); \
        \  \
        \ alter table book_reference \
        \   add constraint book_reference_pk \
        \      primary key (short_uuid); "
  let action conn = execute_ conn sql
  runDB action
