module Registry.Api.Handler.ActionKey.Api where

import Servant

import Registry.Api.Handler.ActionKey.List_POST
import Registry.Model.Context.BaseContext

type ActionKeyAPI = List_POST

actionKeyApi :: Proxy ActionKeyAPI
actionKeyApi = Proxy

actionKeyServer :: ServerT ActionKeyAPI BaseContextM
actionKeyServer = list_POST
