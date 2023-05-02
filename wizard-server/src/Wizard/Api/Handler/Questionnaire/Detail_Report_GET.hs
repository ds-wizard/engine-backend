module Wizard.Api.Handler.Questionnaire.Detail_Report_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Report.Report
import Wizard.Service.Report.ReportService

type Detail_Report_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "report"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Report)

detail_report_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] Report)
detail_report_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getReportByQuestionnaireUuid qtnUuid
