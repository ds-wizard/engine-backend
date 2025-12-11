module Wizard.Database.Mapping.Project.ProjectSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Project.ProjectSimple

instance ToRow ProjectSimple where
  toRow ProjectSimple {..} = [toField uuid, toField name]

instance FromRow ProjectSimple where
  fromRow = do
    uuid <- field
    name <- field
    return $ ProjectSimple {..}

fieldProjectSimple :: RowParser ProjectSimple
fieldProjectSimple = do
  uuid <- field
  name <- field
  return ProjectSimple {..}
