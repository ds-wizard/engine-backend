module Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = simpleParseJSON "_questionnaireDetailDTO"

instance ToJSON QuestionnaireDetailDTO where
  toJSON = simpleToJSON "_questionnaireDetailDTO"
