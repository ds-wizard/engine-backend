module Wizard.Api.Handler.Tenant.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantCreateJM ()
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.TenantService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantCreateDTO
    :> "tenants"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantDTO)

list_POST
  :: Maybe String -> Maybe String -> TenantCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        ia <- isAdmin
        if ia
          then createTenantByAdmin reqDto
          else registerTenant reqDto
