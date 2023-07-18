module Wizard.Api.Handler.Prefab.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Prefab.Api.Resource.Prefab.PrefabJM ()
import Shared.Prefab.Model.Prefab.Prefab
import Shared.Prefab.Service.Prefab.PrefabService
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "prefabs"
    :> QueryParam "type" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [Prefab])

list_GET
  :: Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [Prefab])
list_GET mTokenHeader mServerUrl prefabType =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "type" <$> prefabType]
        getPrefabsFiltered queryParams
