module Wizard.Api.Handler.Questionnaire.Detail_Report_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Report.Report
import Wizard.Service.Report.ReportService

type Detail_Report_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "report"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Report)

detail_report_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Report)
detail_report_GET mTokenHeader qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getReportByQuestionnaireUuid qtnUuid
