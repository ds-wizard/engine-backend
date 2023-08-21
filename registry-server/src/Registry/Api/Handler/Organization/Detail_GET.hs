module Registry.Api.Handler.Organization.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_GET =
  Header "Authorization" String
    :> "organizations"
    :> Capture "orgId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_GET mTokenHeader orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getOrganizationByOrgId orgId
