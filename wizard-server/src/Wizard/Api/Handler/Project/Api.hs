module Wizard.Api.Handler.Project.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Comment.Api
import Wizard.Api.Handler.Project.Detail_Content_PUT
import Wizard.Api.Handler.Project.Detail_DELETE
import Wizard.Api.Handler.Project.Detail_Documents_GET
import Wizard.Api.Handler.Project.Detail_Documents_Preview_GET
import Wizard.Api.Handler.Project.Detail_GET
import Wizard.Api.Handler.Project.Detail_Preview_GET
import Wizard.Api.Handler.Project.Detail_Questionnaire_GET
import Wizard.Api.Handler.Project.Detail_Report_GET
import Wizard.Api.Handler.Project.Detail_Revert_POST
import Wizard.Api.Handler.Project.Detail_Revert_Preview_POST
import Wizard.Api.Handler.Project.Detail_Settings_GET
import Wizard.Api.Handler.Project.Detail_Settings_PUT
import Wizard.Api.Handler.Project.Detail_Share_PUT
import Wizard.Api.Handler.Project.Detail_WS
import Wizard.Api.Handler.Project.Event.Api
import Wizard.Api.Handler.Project.File.Api
import Wizard.Api.Handler.Project.List_GET
import Wizard.Api.Handler.Project.List_POST
import Wizard.Api.Handler.Project.List_POST_CloneUuid
import Wizard.Api.Handler.Project.List_POST_FromTemplate
import Wizard.Api.Handler.Project.Migration.Api
import Wizard.Api.Handler.Project.Tag.Api
import Wizard.Api.Handler.Project.User.Api
import Wizard.Api.Handler.Project.Version.Api
import Wizard.Model.Context.BaseContext

type ProjectAPI =
  Tags "Project"
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
          :<|> CommentAPI
          :<|> EventAPI
          :<|> FileAPI
          :<|> MigrationAPI
          :<|> TagAPI
          :<|> UserAPI
          :<|> VersionAPI
       )

projectApi :: Proxy ProjectAPI
projectApi = Proxy

projectServer :: ServerT ProjectAPI BaseContextM
projectServer =
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
    :<|> commentServer
    :<|> eventServer
    :<|> fileServer
    :<|> migrationServer
    :<|> tagServer
    :<|> userServer
    :<|> versionServer
