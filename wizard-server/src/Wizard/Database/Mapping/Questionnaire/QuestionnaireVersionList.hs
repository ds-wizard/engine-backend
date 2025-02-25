module Wizard.Database.Mapping.Questionnaire.QuestionnaireVersionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Gravatar
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

instance FromRow QuestionnaireVersionList where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    eventUuid <- field
    createdAt <- field
    updatedAt <- field
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
    return $ QuestionnaireVersionList {..}
