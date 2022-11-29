module Registry.Api.Handler.Organization.List_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type List_GET =
  Header "Authorization" String
    :> "organizations"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OrganizationDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [OrganizationDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getOrganizations
