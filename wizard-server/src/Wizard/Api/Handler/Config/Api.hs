module Wizard.Api.Handler.Config.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Config.List_Bootstrap_GET
import Wizard.Model.Context.BaseContext

type ConfigAPI =
  Tags "Config"
    :> List_Bootstrap_GET

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer = list_bootstrap_GET
