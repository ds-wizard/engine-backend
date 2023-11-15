module Wizard.Api.Handler.QuestionnaireAction.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.QuestionnaireAction.Detail_GET
import Wizard.Api.Handler.QuestionnaireAction.Detail_PUT
import Wizard.Api.Handler.QuestionnaireAction.List_GET
import Wizard.Api.Handler.QuestionnaireAction.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireActionAPI =
  Tags "Questionnaire Action"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> Detail_GET
          :<|> Detail_PUT
       )

questionnaireActionApi :: Proxy QuestionnaireActionAPI
questionnaireActionApi = Proxy

questionnaireActionServer :: ServerT QuestionnaireActionAPI BaseContextM
questionnaireActionServer = list_GET :<|> list_suggestions_GET :<|> detail_GET :<|> detail_PUT
