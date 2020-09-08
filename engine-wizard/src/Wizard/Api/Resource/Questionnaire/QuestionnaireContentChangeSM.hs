module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireContentChangeDTO where
  declareNamedSchema = simpleToSchema contentChangeDTO
