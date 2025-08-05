module Wizard.Database.Mapping.Submission.SubmissionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.Submission.Submission ()
import Wizard.Model.Submission.SubmissionList
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

instance FromRow SubmissionList where
  fromRow = do
    uuid <- field
    state <- field
    location <- field
    returnedData <- field
    documentUuid <- field
    createdAt <- field
    updatedAt <- field
    serviceId <- field
    serviceName <- field
    createdByUuid <- field
    createdByFirstName <- field
    createdByLastName <- field
    createdByGravatarHash <- field
    createdByImageUrl <- field
    let createdBy =
          UserSuggestionDTO
            { uuid = createdByUuid
            , firstName = createdByFirstName
            , lastName = createdByLastName
            , gravatarHash = createdByGravatarHash
            , imageUrl = createdByImageUrl
            }
    return $ SubmissionList {..}
