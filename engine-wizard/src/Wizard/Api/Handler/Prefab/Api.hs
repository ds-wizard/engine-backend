module Wizard.Api.Handler.Prefab.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Prefab.List_GET
import Wizard.Model.Context.BaseContext

type PrefabAPI
   = Tags "Prefab"
     :> List_GET

prefabApi :: Proxy PrefabAPI
prefabApi = Proxy

prefabServer :: ServerT PrefabAPI BaseContextM
prefabServer = list_GET
