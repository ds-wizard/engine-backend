module Wizard.Database.Mapping.Submission.SubmissionList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.Submission.Submission ()
import Wizard.Model.Submission.SubmissionList
import WizardLib.Public.Database.Mapping.User.UserSuggestion

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
    createdBy <- fieldUserSuggestion'
    return $ SubmissionList {..}
