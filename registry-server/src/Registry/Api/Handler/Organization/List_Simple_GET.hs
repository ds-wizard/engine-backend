module Registry.Api.Handler.Organization.List_Simple_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

list_simple_GET :: BaseContextM (Headers '[Header "x-trace-uuid" String] [OrganizationSimple])
list_simple_GET = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getSimpleOrganizations
