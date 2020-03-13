module Wizard.Api.Handler.Config.Api where

import Servant

import Wizard.Api.Handler.Config.List_Application_GET
import Wizard.Api.Handler.Config.List_Application_PUT
import Wizard.Api.Handler.Config.List_Client_GET
import Wizard.Model.Context.BaseContext

type ConfigAPI
   = List_Client_GET
     :<|> List_Application_GET
     :<|> List_Application_PUT

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer = list_client_GET :<|> list_application_GET :<|> list_application_PUT
