module Wizard.Api.Handler.Questionnaire.Event.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Questionnaire.Event.Detail_GET
import Wizard.Api.Handler.Questionnaire.Event.List_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireEventAPI
   = Tags "Questionnaire Event"
     :> (List_GET
         :<|> Detail_GET)

questionnaireEventApi :: Proxy QuestionnaireEventAPI
questionnaireEventApi = Proxy

questionnaireEventServer :: ServerT QuestionnaireEventAPI BaseContextM
questionnaireEventServer = list_GET :<|> detail_GET
