module Wizard.Database.Mapping.Project.Comment.ProjectCommentThreadNotification where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.Project.Comment.ProjectCommentThreadNotification
import WizardLib.Public.Model.User.UserSimple

instance FromRow ProjectCommentThreadNotification where
  fromRow = do
    projectUuid <- field
    projectName <- field
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
          UserSimple
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
                UserSimple
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
    return ProjectCommentThreadNotification {..}
