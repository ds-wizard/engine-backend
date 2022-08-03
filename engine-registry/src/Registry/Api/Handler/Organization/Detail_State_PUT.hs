module Registry.Api.Handler.Organization.Detail_State_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_State_PUT
   = ReqBody '[ SafeJSON] OrganizationStateDTO
     :> "organizations"
     :> Capture "orgId" String
     :> "state"
     :> QueryParam "hash" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

detail_state_PUT_Api :: Proxy Detail_State_PUT
detail_state_PUT_Api = Proxy

detail_state_PUT ::
     OrganizationStateDTO
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
detail_state_PUT reqDto orgId mHash =
  runInUnauthService Transactional $ addTraceUuidHeader =<< changeOrganizationState orgId mHash reqDto
