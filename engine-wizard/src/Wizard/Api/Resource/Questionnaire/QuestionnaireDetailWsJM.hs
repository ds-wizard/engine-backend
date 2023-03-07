module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireDetailWsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailWsDTO where
  toJSON = genericToJSON jsonOptions
