module Wizard.Database.Mapping.User.UserSubmissionProp where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.User.UserSubmissionProp

instance ToRow UserSubmissionProp where
  toRow UserSubmissionProp {..} =
    [ toField userUuid
    , toField serviceId
    , toJSONField values
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow UserSubmissionProp where
  fromRow = do
    userUuid <- field
    serviceId <- field
    values <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ UserSubmissionProp {..}
