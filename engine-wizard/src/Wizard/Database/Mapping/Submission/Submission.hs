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
    [ toField _submissionUuid
    , toField _submissionState
    , toField _submissionLocation
    , toField _submissionReturnedData
    , toField _submissionServiceId
    , toField _submissionDocumentUuid
    , toField _submissionCreatedBy
    , toField _submissionCreatedAt
    , toField _submissionUpdatedAt
    ]

instance FromRow Submission where
  fromRow = do
    _submissionUuid <- field
    _submissionState <- field
    _submissionLocation <- field
    _submissionReturnedData <- field
    _submissionServiceId <- field
    _submissionDocumentUuid <- field
    _submissionCreatedBy <- field
    _submissionCreatedAt <- field
    _submissionUpdatedAt <- field
    return $ Submission {..}
