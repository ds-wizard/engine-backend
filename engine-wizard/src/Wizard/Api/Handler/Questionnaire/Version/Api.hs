module Wizard.Api.Handler.Questionnaire.Version.Api where

import Servant

import Wizard.Api.Handler.Questionnaire.Version.Detail_DELETE
import Wizard.Api.Handler.Questionnaire.Version.Detail_PUT
import Wizard.Api.Handler.Questionnaire.Version.List_GET
import Wizard.Api.Handler.Questionnaire.Version.List_POST
import Wizard.Model.Context.BaseContext

type QuestionnaireVersionAPI
   = List_GET
     :<|> List_POST
     :<|> Detail_PUT
     :<|> Detail_DELETE

questionnaireVersionApi :: Proxy QuestionnaireVersionAPI
questionnaireVersionApi = Proxy

questionnaireVersionServer :: ServerT QuestionnaireVersionAPI BaseContextM
questionnaireVersionServer = list_GET :<|> list_POST :<|> detail_PUT :<|> detail_DELETE
