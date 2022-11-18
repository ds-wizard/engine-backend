module Wizard.Database.Mapping.Submission.Submission where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Common
import Wizard.Model.Submission.Submission

instance ToField SubmissionState where
  toField = toFieldGenericEnum

instance FromField SubmissionState where
  fromField = fromFieldGenericEnum

instance ToRow Submission where
  toRow Submission {..} =
    [ toField uuid
    , toField state
    , toField location
    , toField returnedData
    , toField serviceId
    , toField documentUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField appUuid
    ]

instance FromRow Submission where
  fromRow = do
    uuid <- field
    state <- field
    location <- field
    returnedData <- field
    serviceId <- field
    documentUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    appUuid <- field
    return $ Submission {..}
