module Wizard.Api.Handler.Usage.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Usage.List_Current_GET
import Wizard.Model.Context.BaseContext

type UsageAPI =
  Tags "Usage"
    :> List_Current_GET

usageApi :: Proxy UsageAPI
usageApi = Proxy

usageServer :: ServerT UsageAPI BaseContextM
usageServer = list_current_GET
