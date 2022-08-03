module Wizard.Api.Handler.App.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "apps"
     :> QueryParam "q" String
     :> QueryParam "enabled" Bool
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page AppDTO))

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page AppDTO))
list_GET mTokenHeader mServerUrl mQuery mEnabled mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
    addTraceUuidHeader =<< getAppsPage mQuery mEnabled (Pageable mPage mSize) (parseSortQuery mSort)
