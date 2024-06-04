module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireCommentThreadDTO where
  declareNamedSchema = toSwagger cmtQ1_t1Dto

instance ToSchema QuestionnaireCommentDTO where
  declareNamedSchema = toSwagger cmtQ1_t1_1Dto
