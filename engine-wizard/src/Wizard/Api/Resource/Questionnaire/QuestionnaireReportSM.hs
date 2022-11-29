module Wizard.Api.Resource.Questionnaire.QuestionnaireReportSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportJM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Database.Migration.Development.Report.Data.Reports

instance ToSchema QuestionnaireReportDTO where
  declareNamedSchema = toSwagger questionnaireReport
