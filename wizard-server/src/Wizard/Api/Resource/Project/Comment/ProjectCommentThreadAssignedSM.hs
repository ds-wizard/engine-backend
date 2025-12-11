module Wizard.Api.Resource.Project.Comment.ProjectCommentThreadAssignedSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectCommentThreadAssigned where
  declareNamedSchema = toSwagger cmtAssigned
