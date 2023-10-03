module Registry.Api.Handler.Config.Api where

import Servant

import Registry.Api.Handler.Config.List_Bootstrap_GET
import Registry.Model.Context.BaseContext

type ConfigAPI =
  List_Bootstrap_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer = list_bootstrap_GET
