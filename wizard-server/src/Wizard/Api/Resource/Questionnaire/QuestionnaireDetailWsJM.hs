module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireDetailWsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailWsDTO where
  toJSON = genericToJSON jsonOptions
