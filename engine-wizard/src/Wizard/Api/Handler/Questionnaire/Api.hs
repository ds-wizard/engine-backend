module Wizard.Api.Handler.Questionnaire.Api where

import Servant

import Wizard.Api.Handler.Questionnaire.Detail_DELETE
import Wizard.Api.Handler.Questionnaire.Detail_Documents_Preview_GET
import Wizard.Api.Handler.Questionnaire.Detail_GET
import Wizard.Api.Handler.Questionnaire.Detail_PUT
import Wizard.Api.Handler.Questionnaire.Detail_Report_GET
import Wizard.Api.Handler.Questionnaire.Detail_Report_Preview_POST
import Wizard.Api.Handler.Questionnaire.List_GET
import Wizard.Api.Handler.Questionnaire.List_POST
import Wizard.Api.Handler.Questionnaire.List_POST_CloneUuid
import Wizard.Api.Handler.Questionnaire.List_Public_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireAPI
   = List_GET
     :<|> List_POST
     :<|> List_POST_CloneUuid
     :<|> List_Public_GET
     :<|> Detail_GET
     :<|> Detail_PUT
     :<|> Detail_DELETE
     :<|> Detail_Report_GET
     :<|> Detail_Report_Preview_POST
     :<|> Detail_Documents_Preview_GET

questionnaireApi :: Proxy QuestionnaireAPI
questionnaireApi = Proxy

questionnaireServer :: ServerT QuestionnaireAPI BaseContextM
questionnaireServer =
  list_GET :<|> list_POST :<|> list_POST_CloneUuid :<|> list_public_GET :<|> detail_GET :<|> detail_PUT :<|>
  detail_DELETE :<|>
  detail_report_GET :<|>
  detail_report_preview_POST :<|>
  detail_documents_preview_GET
