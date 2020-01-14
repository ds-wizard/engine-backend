module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = simpleParseJSON "_questionnaireDetailDTO"

instance ToJSON QuestionnaireDetailDTO where
  toJSON = simpleToJSON "_questionnaireDetailDTO"
