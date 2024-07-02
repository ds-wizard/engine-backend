module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

instance FromJSON QuestionnaireDetailReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailReportDTO where
  toJSON = genericToJSON jsonOptions
