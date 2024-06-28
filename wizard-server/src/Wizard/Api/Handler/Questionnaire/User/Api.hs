module Wizard.Api.Handler.Questionnaire.User.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Questionnaire.User.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireUserAPI =
  Tags "Questionnaire User"
    :> List_Suggestions_GET

questionnaireUserApi :: Proxy QuestionnaireUserAPI
questionnaireUserApi = Proxy

questionnaireUserServer :: ServerT QuestionnaireUserAPI BaseContextM
questionnaireUserServer = list_suggestions_GET
