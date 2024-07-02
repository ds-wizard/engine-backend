module Wizard.Api.Handler.Questionnaire.Detail_Report_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Report.ReportService

type Detail_Report_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "report"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailReportDTO)

detail_report_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailReportDTO)
detail_report_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getReportByQuestionnaireUuid qtnUuid
