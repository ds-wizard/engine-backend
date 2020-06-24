module Wizard.Api.Handler.Questionnaire.Detail_Report_Preview_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Report.ReportService

type Detail_Report_Preview_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "report"
     :> "preview"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] ReportDTO)

detail_report_preview_POST ::
     Maybe String
  -> QuestionnaireChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] ReportDTO)
detail_report_preview_POST mTokenHeader reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getPreviewOfReportByQuestionnaireUuid qtnUuid reqDto
