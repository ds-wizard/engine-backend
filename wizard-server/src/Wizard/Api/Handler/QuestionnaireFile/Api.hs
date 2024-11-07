module Wizard.Api.Handler.QuestionnaireFile.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.QuestionnaireFile.List_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireFileAPI =
  Tags "Questionnaire File"
    :> List_GET

questionnaireFileApi :: Proxy QuestionnaireFileAPI
questionnaireFileApi = Proxy

questionnaireFileServer :: ServerT QuestionnaireFileAPI BaseContextM
questionnaireFileServer =
  list_GET
