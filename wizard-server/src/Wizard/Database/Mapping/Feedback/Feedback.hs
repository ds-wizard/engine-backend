module Wizard.Database.Mapping.Feedback.Feedback where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Feedback.Feedback

instance ToRow Feedback where
  toRow Feedback {..} =
    [ toField uuid
    , toField issueId
    , toField questionUuid
    , toField packageId
    , toField title
    , toField content
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    ]

instance FromRow Feedback where
  fromRow = do
    uuid <- field
    issueId <- field
    questionUuid <- field
    packageId <- field
    title <- field
    content <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    return $ Feedback {..}
