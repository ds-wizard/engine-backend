module Wizard.Api.Handler.Info.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Info.List_GET
import Wizard.Api.Handler.Info.List_Robots_GET
import Wizard.Model.Context.BaseContext

type InfoAPI =
  Tags "Info"
    :> (List_GET :<|> List_Robots_GET)

infoApi :: Proxy InfoAPI
infoApi = Proxy

infoServer :: ServerT InfoAPI BaseContextM
infoServer = list_GET :<|> list_robots_GET
