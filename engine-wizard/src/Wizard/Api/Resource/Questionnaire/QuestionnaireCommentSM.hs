module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments

instance ToSchema QuestionnaireCommentThreadDTO where
  declareNamedSchema = simpleToSchema cmtQ1_t1Dto

instance ToSchema QuestionnaireCommentDTO where
  declareNamedSchema = simpleToSchema cmtQ1_t1_1Dto
