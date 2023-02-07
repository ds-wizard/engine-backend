module Wizard.Api.Handler.QuestionnaireImporter.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.QuestionnaireImporter.Detail_GET
import Wizard.Api.Handler.QuestionnaireImporter.Detail_PUT
import Wizard.Api.Handler.QuestionnaireImporter.List_GET
import Wizard.Api.Handler.QuestionnaireImporter.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireImporterAPI =
  Tags "Questionnaire Importer"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> Detail_GET
          :<|> Detail_PUT
       )

questionnaireImporterApi :: Proxy QuestionnaireImporterAPI
questionnaireImporterApi = Proxy

questionnaireImporterServer :: ServerT QuestionnaireImporterAPI BaseContextM
questionnaireImporterServer = list_GET :<|> list_suggestions_GET :<|> detail_GET :<|> detail_PUT
