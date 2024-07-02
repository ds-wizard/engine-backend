module Wizard.Api.Handler.Questionnaire.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Questionnaire.Comment.Api
import Wizard.Api.Handler.Questionnaire.Detail_Content_PUT
import Wizard.Api.Handler.Questionnaire.Detail_DELETE
import Wizard.Api.Handler.Questionnaire.Detail_Documents_GET
import Wizard.Api.Handler.Questionnaire.Detail_Documents_Preview_GET
import Wizard.Api.Handler.Questionnaire.Detail_GET
import Wizard.Api.Handler.Questionnaire.Detail_Preview_GET
import Wizard.Api.Handler.Questionnaire.Detail_Questionnaire_GET
import Wizard.Api.Handler.Questionnaire.Detail_Report_GET
import Wizard.Api.Handler.Questionnaire.Detail_Revert_POST
import Wizard.Api.Handler.Questionnaire.Detail_Revert_Preview_POST
import Wizard.Api.Handler.Questionnaire.Detail_Settings_GET
import Wizard.Api.Handler.Questionnaire.Detail_Settings_PUT
import Wizard.Api.Handler.Questionnaire.Detail_Share_PUT
import Wizard.Api.Handler.Questionnaire.Detail_WS
import Wizard.Api.Handler.Questionnaire.List_GET
import Wizard.Api.Handler.Questionnaire.List_POST
import Wizard.Api.Handler.Questionnaire.List_POST_CloneUuid
import Wizard.Api.Handler.Questionnaire.List_POST_FromTemplate
import Wizard.Api.Handler.Questionnaire.ProjectTag.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireAPI =
  Tags "Questionnaire"
    :> ( List_GET
          :<|> List_POST
          :<|> List_POST_FromTemplate
          :<|> List_POST_CloneUuid
          :<|> Detail_GET
          :<|> Detail_Questionnaire_GET
          :<|> Detail_Preview_GET
          :<|> Detail_Settings_GET
          :<|> Detail_Settings_PUT
          :<|> Detail_Share_PUT
          :<|> Detail_DELETE
          :<|> Detail_Content_PUT
          :<|> Detail_Report_GET
          :<|> Detail_Documents_GET
          :<|> Detail_Documents_Preview_GET
          :<|> Detail_WS
          :<|> Detail_Revert_POST
          :<|> Detail_Revert_Preview_POST
          :<|> List_Suggestions_GET
          :<|> QuestionnaireCommentAPI
       )

questionnaireApi :: Proxy QuestionnaireAPI
questionnaireApi = Proxy

questionnaireServer :: ServerT QuestionnaireAPI BaseContextM
questionnaireServer =
  list_GET
    :<|> list_POST
    :<|> list_POST_FromTemplate
    :<|> list_POST_CloneUuid
    :<|> detail_GET
    :<|> detail_questionnaire_GET
    :<|> detail_preview_GET
    :<|> detail_settings_GET
    :<|> detail_settings_PUT
    :<|> detail_share_PUT
    :<|> detail_DELETE
    :<|> detail_content_PUT
    :<|> detail_report_GET
    :<|> detail_documents_GET
    :<|> detail_documents_preview_GET
    :<|> detail_WS
    :<|> detail_revert_POST
    :<|> detail_revert_preview_POST
    :<|> list_suggestions_GET
    :<|> questionnaireCommentServer
