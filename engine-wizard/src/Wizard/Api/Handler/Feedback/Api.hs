module Wizard.Api.Handler.Feedback.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Feedback.Detail_GET
import Wizard.Api.Handler.Feedback.List_GET
import Wizard.Api.Handler.Feedback.List_POST
import Wizard.Model.Context.BaseContext

type FeedbackAPI
   = Tags "Feedback"
     :> (List_GET
         :<|> List_POST
         :<|> Detail_GET)

feedbackApi :: Proxy FeedbackAPI
feedbackApi = Proxy

feedbackServer :: ServerT FeedbackAPI BaseContextM
feedbackServer = list_GET :<|> list_POST :<|> detail_GET
