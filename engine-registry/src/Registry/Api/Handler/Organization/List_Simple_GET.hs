module Registry.Api.Handler.Organization.List_Simple_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()

type List_Simple_GET
   = "organizations"
     :> "simple"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [OrganizationSimpleDTO])

list_simple_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] [OrganizationSimpleDTO])
list_simple_GET = runInUnauthService $ addTraceUuidHeader =<< getSimpleOrganizations
