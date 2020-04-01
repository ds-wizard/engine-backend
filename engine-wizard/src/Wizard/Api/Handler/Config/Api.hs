module Wizard.Api.Handler.Config.Api where

import Servant

import Wizard.Api.Handler.Config.List_App_GET
import Wizard.Api.Handler.Config.List_App_PUT
import Wizard.Api.Handler.Config.List_Bootstrap_GET
import Wizard.Model.Context.BaseContext

type ConfigAPI
   = List_App_GET
     :<|> List_App_PUT
     :<|> List_Bootstrap_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer = list_app_GET :<|> list_app_PUT :<|> list_bootstrap_GET
