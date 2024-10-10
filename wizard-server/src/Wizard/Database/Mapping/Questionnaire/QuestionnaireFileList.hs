module Wizard.Database.Mapping.Questionnaire.QuestionnaireFileList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Gravatar
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireSimple
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

instance FromRow QuestionnaireFileList where
  fromRow = do
    uuid <- field
    fileName <- field
    contentType <- field
    fileSize <- field
    createdAt <- field
    questionnaireUuid <- field
    questionnaireName <- field
    let questionnaire =
          QuestionnaireSimple
            { uuid = questionnaireUuid
            , name = questionnaireName
            }
    createdByUuid <- field
    createdByFirstName <- field
    createdByLastName <- field
    createdByEmail <- field
    createdByImageUrl <- field
    let createdBy =
          case (createdByUuid, createdByFirstName, createdByLastName, createdByEmail, createdByImageUrl) of
            (Just uuid, Just firstName, Just lastName, Just email, imageUrl) ->
              let gravatarHash = createGravatarHash email
               in Just UserSuggestionDTO {..}
            _ -> Nothing
    return $ QuestionnaireFileList {..}
