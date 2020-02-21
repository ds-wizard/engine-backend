module Wizard.Api.Handler.Config.Api where

import Servant

import Wizard.Api.Handler.Config.List_GET
import Wizard.Model.Context.BaseContext

type ConfigAPI = List_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer = list_GET
