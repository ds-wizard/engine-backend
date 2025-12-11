module Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Model.Project.Comment.ProjectCommentList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectCommentThreadList where
  declareNamedSchema = toSwagger cmtQ1_t1Dto

instance ToSchema ProjectCommentList where
  declareNamedSchema = toSwagger cmtQ1_t1_1Dto
