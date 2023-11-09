module Wizard.Database.Mapping.Questionnaire.QuestionnaireComment where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToRow QuestionnaireComment where
  toRow QuestionnaireComment {..} =
    [ toField uuid
    , toField text
    , toField threadUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    ]

instance FromRow QuestionnaireComment where
  fromRow = do
    uuid <- field
    text <- field
    threadUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    return QuestionnaireComment {..}
