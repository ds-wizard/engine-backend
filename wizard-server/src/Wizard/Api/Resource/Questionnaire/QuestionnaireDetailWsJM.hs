module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON QuestionnaireDetailWsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailWsDTO where
  toJSON = genericToJSON jsonOptions
