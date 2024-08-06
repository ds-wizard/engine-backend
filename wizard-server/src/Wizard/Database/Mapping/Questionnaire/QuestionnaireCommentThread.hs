module Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Util.Date
import Shared.Common.Util.String (splitOn)
import Shared.Common.Util.Uuid
import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToRow QuestionnaireCommentThread where
  toRow QuestionnaireCommentThread {..} =
    [ toField uuid
    , toField path
    , toField resolved
    , toField private
    , toField questionnaireUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    , toField assignedTo
    , toField assignedBy
    , toField notificationRequired
    ]

instance FromRow QuestionnaireCommentThread where
  fromRow = do
    uuid <- field
    path <- field
    resolved <- field
    private <- field
    questionnaireUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    assignedTo <- field
    assignedBy <- field
    notificationRequired <- field
    commentsArray <- fromPGArray <$> field
    let comments = fmap parseComment commentsArray
    return $ QuestionnaireCommentThread {..}

parseComment :: String -> QuestionnaireComment
parseComment commentS =
  let parts = splitOn ":::::" commentS
   in QuestionnaireComment
        { uuid = u' (head parts)
        , text = parts !! 1
        , threadUuid = u' (parts !! 2)
        , tenantUuid = u' (parts !! 3)
        , createdBy =
            case parts !! 4 of
              "" -> Nothing
              u -> Just . u' $ u
        , createdAt = parsePostgresDateTime' $ parts !! 5
        , updatedAt = parsePostgresDateTime' $ parts !! 6
        }
