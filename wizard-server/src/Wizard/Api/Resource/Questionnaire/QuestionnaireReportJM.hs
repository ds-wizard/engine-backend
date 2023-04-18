module Wizard.Api.Resource.Questionnaire.QuestionnaireReportJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Report.ReportJM ()

instance FromJSON QuestionnaireReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireReportDTO where
  toJSON = genericToJSON jsonOptions
