module Shared.Common.Api.Handler.Root.Api where

import Servant

import Shared.Common.Api.Handler.Root.List_GET
import Shared.Common.Model.Context.BaseContext

type RootAPI = List_GET

rootApi :: Proxy RootAPI
rootApi = Proxy

rootServer :: BaseContextC s sc m => String -> ServerT RootAPI m
rootServer = list_GET
