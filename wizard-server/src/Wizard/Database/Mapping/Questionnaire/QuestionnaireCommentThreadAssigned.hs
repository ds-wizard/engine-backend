module Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadAssigned where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Gravatar
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import WizardLib.Public.Model.User.UserSuggestion

instance FromRow QuestionnaireCommentThreadAssigned where
  fromRow = do
    questionnaireUuid <- field
    questionnaireName <- field
    commentThreadUuid <- field
    path <- field
    resolved <- field
    private <- field
    updatedAt <- field
    text <- field
    mCreatedByUuid <- fieldWith (optionalField fromField)
    mCreatedByFirstName <- fieldWith (optionalField fromField)
    mCreatedByLastName <- fieldWith (optionalField fromField)
    mCreatedByEmail <- fieldWith (optionalField fromField)
    mCreatedByImageUrl <- fieldWith (optionalField fromField)
    let createdBy =
          case (mCreatedByUuid, mCreatedByFirstName, mCreatedByLastName, mCreatedByEmail) of
            (Just createdByUuid, Just createdByFirstName, Just createdByLastName, Just createdByEmail) ->
              Just $
                UserSuggestion
                  { uuid = createdByUuid
                  , firstName = createdByFirstName
                  , lastName = createdByLastName
                  , imageUrl = mCreatedByImageUrl
                  , gravatarHash = createGravatarHash createdByEmail
                  }
            _ -> Nothing
    return QuestionnaireCommentThreadAssigned {..}
