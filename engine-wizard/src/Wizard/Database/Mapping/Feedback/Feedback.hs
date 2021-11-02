module Wizard.Database.Mapping.Feedback.Feedback where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Feedback.Feedback

instance ToRow Feedback where
  toRow Feedback {..} =
    [ toField _feedbackUuid
    , toField _feedbackIssueId
    , toField _feedbackQuestionUuid
    , toField _feedbackPackageId
    , toField _feedbackTitle
    , toField _feedbackContent
    , toField _feedbackCreatedAt
    , toField _feedbackUpdatedAt
    , toField _feedbackAppUuid
    ]

instance FromRow Feedback where
  fromRow = do
    _feedbackUuid <- field
    _feedbackIssueId <- field
    _feedbackQuestionUuid <- field
    _feedbackPackageId <- field
    _feedbackTitle <- field
    _feedbackContent <- field
    _feedbackCreatedAt <- field
    _feedbackUpdatedAt <- field
    _feedbackAppUuid <- field
    return $ Feedback {..}
