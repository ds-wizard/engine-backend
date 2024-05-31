module Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireShareChangeDTO where
  declareNamedSchema = toSwagger questionnaire1EditedShareChange
