module Registry.Api.Handler.Organization.Detail_State_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Api.Resource.Organization.OrganizationStateDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

detail_state_PUT
  :: OrganizationStateDTO
  -> String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_state_PUT reqDto orgId hash =
  runInUnauthService Transactional $ addTraceUuidHeader =<< changeOrganizationState orgId hash reqDto
