module Registry.Api.Web where

import Servant

import Registry.Api.Handler.Api
import Registry.Api.Handler.Swagger.Api
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Shared.Common.Bootstrap.Web

type WebAPI =
  SwaggerAPI
    :<|> ApplicationAPI

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: BaseContext -> Server WebAPI
webServer baseContext = swaggerServer :<|> hoistServer applicationApi (convert baseContext runBaseContextM) applicationServer
