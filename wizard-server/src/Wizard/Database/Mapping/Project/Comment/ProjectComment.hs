module Wizard.Database.Mapping.Project.Comment.ProjectComment where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Project.Comment.ProjectComment

instance ToRow ProjectComment where
  toRow ProjectComment {..} =
    [ toField uuid
    , toField text
    , toField threadUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    ]

instance FromRow ProjectComment where
  fromRow = do
    uuid <- field
    text <- field
    threadUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    return ProjectComment {..}
