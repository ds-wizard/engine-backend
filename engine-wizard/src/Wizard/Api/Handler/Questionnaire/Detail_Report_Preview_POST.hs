module Wizard.Api.Handler.Questionnaire.Detail_Report_Preview_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Report.Report
import Wizard.Service.Report.ReportService

type Detail_Report_Preview_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireContentChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "report"
     :> "preview"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Report)

detail_report_preview_POST ::
     Maybe String
  -> QuestionnaireContentChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Report)
detail_report_preview_POST mTokenHeader reqDto qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto
