module Wizard.Api.Handler.Cache.Api where

import Servant

import Wizard.Api.Handler.Cache.List_DELETE
import Wizard.Model.Context.BaseContext

type CacheAPI = List_DELETE

cacheApi :: Proxy CacheAPI
cacheApi = Proxy

cacheServer :: ServerT CacheAPI BaseContextM
cacheServer = list_DELETE
