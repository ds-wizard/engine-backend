module RegistryLib.Api.Handler.Organization.Detail_State_PUT where

import Servant

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Api.Resource.Organization.OrganizationStateDTO
import RegistryLib.Api.Resource.Organization.OrganizationStateJM ()
import Shared.Common.Api.Handler.Common

type Detail_State_PUT =
  ReqBody '[SafeJSON] OrganizationStateDTO
    :> "organizations"
    :> Capture "orgId" String
    :> "state"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_state_PUT_Api :: Proxy Detail_State_PUT
detail_state_PUT_Api = Proxy
