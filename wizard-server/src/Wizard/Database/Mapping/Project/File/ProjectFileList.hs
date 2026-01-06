module Wizard.Database.Mapping.Project.File.ProjectFileList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Gravatar
import Wizard.Database.Mapping.Project.ProjectSimple
import Wizard.Model.Project.File.ProjectFileList
import WizardLib.Public.Model.User.UserSuggestion

instance FromRow ProjectFileList where
  fromRow = do
    uuid <- field
    fileName <- field
    contentType <- field
    fileSize <- field
    createdAt <- field
    project <- fieldProjectSimple
    createdByUuid <- field
    createdByFirstName <- field
    createdByLastName <- field
    createdByEmail <- field
    createdByImageUrl <- field
    let createdBy =
          case (createdByUuid, createdByFirstName, createdByLastName, createdByEmail, createdByImageUrl) of
            (Just uuid, Just firstName, Just lastName, Just email, imageUrl) ->
              let gravatarHash = createGravatarHash email
               in Just UserSuggestion {..}
            _ -> Nothing
    return $ ProjectFileList {..}
