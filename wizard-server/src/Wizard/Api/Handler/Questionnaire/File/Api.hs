module Wizard.Api.Handler.Questionnaire.File.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Questionnaire.File.Detail_DELETE
import Wizard.Api.Handler.Questionnaire.File.Detail_Download_GET
import Wizard.Api.Handler.Questionnaire.File.List_GET
import Wizard.Api.Handler.Questionnaire.File.List_POST
import Wizard.Model.Context.BaseContext

type QuestionnaireFileAPI =
  Tags "Questionnaire File"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_DELETE
          :<|> Detail_Download_GET
       )

questionnaireFileApi :: Proxy QuestionnaireFileAPI
questionnaireFileApi = Proxy

questionnaireFileServer :: ServerT QuestionnaireFileAPI BaseContextM
questionnaireFileServer =
  list_GET
    :<|> list_POST
    :<|> detail_DELETE
    :<|> detail_download_GET
