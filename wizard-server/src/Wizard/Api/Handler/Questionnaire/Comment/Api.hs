module Wizard.Api.Handler.Questionnaire.Comment.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Questionnaire.Comment.List_GET
import Wizard.Model.Context.BaseContext

type QuestionnaireCommentAPI =
  Tags "Questionnaire Comment"
    :> List_GET

questionnaireCommentApi :: Proxy QuestionnaireCommentAPI
questionnaireCommentApi = Proxy

questionnaireCommentServer :: ServerT QuestionnaireCommentAPI BaseContextM
questionnaireCommentServer = list_GET
