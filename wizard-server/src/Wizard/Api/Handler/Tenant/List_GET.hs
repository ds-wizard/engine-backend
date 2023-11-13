module Wizard.Api.Handler.Tenant.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.TenantService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> QueryParam "q" String
    :> QueryParam "enabled" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page TenantDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page TenantDTO))
list_GET mTokenHeader mServerUrl mQuery mEnabled mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTenantsPage mQuery mEnabled (Pageable mPage mSize) (parseSortQuery mSort)
