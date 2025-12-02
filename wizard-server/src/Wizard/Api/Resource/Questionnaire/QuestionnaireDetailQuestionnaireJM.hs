module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireDetailQuestionnaireDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailQuestionnaireDTO where
  toJSON = genericToJSON jsonOptions
