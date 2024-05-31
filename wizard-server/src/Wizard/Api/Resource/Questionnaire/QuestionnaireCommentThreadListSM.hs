module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireCommentThreadList where
  declareNamedSchema = toSwagger cmtQ1_t1Dto

instance ToSchema QuestionnaireCommentList where
  declareNamedSchema = toSwagger cmtQ1_t1_1Dto
