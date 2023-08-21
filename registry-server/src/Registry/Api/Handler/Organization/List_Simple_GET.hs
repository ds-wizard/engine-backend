module Registry.Api.Handler.Organization.List_Simple_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

list_simple_GET :: BaseContextM (Headers '[Header "x-trace-uuid" String] [OrganizationSimpleDTO])
list_simple_GET = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getSimpleOrganizations
