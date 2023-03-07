module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireDetailWsDTO where
  declareNamedSchema = toSwagger (toDetailWsDTO questionnaire1 Nothing Nothing [])
