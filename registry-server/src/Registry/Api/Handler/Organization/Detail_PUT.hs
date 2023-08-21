module Registry.Api.Handler.Organization.Detail_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationChangeJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_PUT =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] OrganizationChangeDTO
    :> "organizations"
    :> Capture "orgId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_PUT
  :: Maybe String
  -> OrganizationChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_PUT mTokenHeader reqDto orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyOrganization orgId reqDto
