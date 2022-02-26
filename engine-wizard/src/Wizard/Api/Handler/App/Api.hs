module Wizard.Api.Handler.App.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.App.List_GET
import Wizard.Api.Handler.App.List_POST
import Wizard.Model.Context.BaseContext

type AppAPI
   = Tags "App"
     :> List_GET
     :<|> List_POST

appApi :: Proxy AppAPI
appApi = Proxy

appServer :: ServerT AppAPI BaseContextM
appServer = list_GET :<|> list_POST
