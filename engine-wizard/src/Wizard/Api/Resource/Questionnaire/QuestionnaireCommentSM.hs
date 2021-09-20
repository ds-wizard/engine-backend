module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToSchema QuestionnaireCommentThread where
  declareNamedSchema = simpleToSchema' "_questionnaireCommentThread" cmtQ1_t1

instance ToSchema QuestionnaireComment where
  declareNamedSchema = simpleToSchema' "_questionnaireComment" cmtQ1_t1_1
