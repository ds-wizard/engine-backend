module Wizard.Api.Handler.Feedback.Api where

import Servant

import Wizard.Api.Handler.Feedback.Detail_GET
import Wizard.Api.Handler.Feedback.List_GET
import Wizard.Api.Handler.Feedback.List_POST
import Wizard.Api.Handler.Feedback.List_Synchronization_GET
import Wizard.Model.Context.BaseContext

type FeedbackAPI
   = List_GET
     :<|> List_POST
     :<|> List_Synchronization_GET
     :<|> Detail_GET

feedbackApi :: Proxy FeedbackAPI
feedbackApi = Proxy

feedbackServer :: ServerT FeedbackAPI BaseContextM
feedbackServer = list_GET :<|> list_POST :<|> list_synchronization_GET :<|> detail_GET
