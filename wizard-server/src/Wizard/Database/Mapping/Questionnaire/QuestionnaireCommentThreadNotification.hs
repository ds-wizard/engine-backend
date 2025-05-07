module Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadNotification where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification
import Wizard.Model.User.UserSuggestion

instance FromRow QuestionnaireCommentThreadNotification where
  fromRow = do
    questionnaireUuid <- field
    questionnaireName <- field
    tenantUuid <- field
    commentThreadUuid <- field
    path <- field
    resolved <- field
    private <- field
    assignedToUuid <- field
    assignedToFirstName <- field
    assignedToLastName <- field
    assignedToEmail <- field
    let assignedTo =
          UserSuggestion
            { uuid = assignedToUuid
            , firstName = assignedToFirstName
            , lastName = assignedToLastName
            , imageUrl = Nothing
            , email = assignedToEmail
            }
    mAssignedByUuid <- fieldWith (optionalField fromField)
    mAssignedByFirstName <- fieldWith (optionalField fromField)
    mAssignedByLastName <- fieldWith (optionalField fromField)
    mAssignedByEmail <- fieldWith (optionalField fromField)
    let assignedBy =
          case (mAssignedByUuid, mAssignedByFirstName, mAssignedByLastName, mAssignedByEmail) of
            (Just assignedByUuid, Just assignedByFirstName, Just assignedByLastName, Just assignedByEmail) ->
              Just $
                UserSuggestion
                  { uuid = assignedByUuid
                  , firstName = assignedByFirstName
                  , lastName = assignedByLastName
                  , imageUrl = Nothing
                  , email = assignedByEmail
                  }
            _ -> Nothing
    text <- field
    clientUrl <- field
    appTitle <- field
    logoUrl <- field
    primaryColor <- field
    illustrationsColor <- field
    supportEmail <- field
    mailConfigUuid <- field
    return QuestionnaireCommentThreadNotification {..}
