module Registry.Api.Handler.Info.Api where

import Servant

import Registry.Api.Handler.Info.List_GET
import Registry.Model.Context.BaseContext

type InfoAPI = List_GET

infoApi :: Proxy InfoAPI
infoApi = Proxy

infoServer :: ServerT InfoAPI BaseContextM
infoServer = list_GET
