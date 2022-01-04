module Wizard.Api.Handler.Cache.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Cache.List_DELETE
import Wizard.Api.Handler.Cache.List_KnowledgeModel_POST
import Wizard.Model.Context.BaseContext

type CacheAPI
   = Tags "Cache"
     :> (List_DELETE
         :<|> List_KnowledgeModel_POST)

cacheApi :: Proxy CacheAPI
cacheApi = Proxy

cacheServer :: ServerT CacheAPI BaseContextM
cacheServer = list_DELETE :<|> list_knowledgeModel_POST
