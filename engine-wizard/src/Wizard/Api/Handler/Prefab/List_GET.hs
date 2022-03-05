module Wizard.Api.Handler.Prefab.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Prefab.PrefabJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Prefab.Prefab
import Wizard.Service.Prefab.PrefabService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "prefabs"
     :> QueryParam "type" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [Prefab])

list_GET ::
     Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [Prefab])
list_GET mTokenHeader mServerUrl prefabType =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "type" <$> prefabType]
      getPrefabsFiltered queryParams
