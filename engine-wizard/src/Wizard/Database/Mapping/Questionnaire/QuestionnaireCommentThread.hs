module Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Util.Date
import Shared.Util.String (splitOn)
import Shared.Util.Uuid
import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToRow QuestionnaireCommentThread where
  toRow QuestionnaireCommentThread {..} =
    [ toField _questionnaireCommentThreadUuid
    , toField _questionnaireCommentThreadPath
    , toField _questionnaireCommentThreadResolved
    , toField _questionnaireCommentThreadPrivate
    , toField _questionnaireCommentThreadQuestionnaireUuid
    , toField _questionnaireCommentThreadCreatedBy
    , toField _questionnaireCommentThreadCreatedAt
    , toField _questionnaireCommentThreadUpdatedAt
    ]

instance FromRow QuestionnaireCommentThread where
  fromRow = do
    _questionnaireCommentThreadUuid <- field
    _questionnaireCommentThreadPath <- field
    _questionnaireCommentThreadResolved <- field
    _questionnaireCommentThreadPrivate <- field
    _questionnaireCommentThreadQuestionnaireUuid <- field
    _questionnaireCommentThreadCreatedBy <- field
    _questionnaireCommentThreadCreatedAt <- field
    _questionnaireCommentThreadUpdatedAt <- field
    comments <- fromPGArray <$> field
    let _questionnaireCommentThreadComments = fmap parseComment comments
    return $ QuestionnaireCommentThread {..}
    where
      parseComment :: String -> QuestionnaireComment
      parseComment parseComment =
        let parts = splitOn ":::::" parseComment
         in QuestionnaireComment
              { _questionnaireCommentUuid = u' (head parts)
              , _questionnaireCommentText = parts !! 1
              , _questionnaireCommentThreadUuid = u' (parts !! 2)
              , _questionnaireCommentCreatedBy =
                  case parts !! 3 of
                    "" -> Nothing
                    uuid -> Just . u' $ uuid
              , _questionnaireCommentCreatedAt = parsePostgresDateTime $ parts !! 4
              , _questionnaireCommentUpdatedAt = parsePostgresDateTime $ parts !! 5
              }
