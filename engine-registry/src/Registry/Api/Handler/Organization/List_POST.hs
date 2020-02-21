module Registry.Api.Handler.Organization.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationCreateJM ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common

type List_POST
   = ReqBody '[ SafeJSON] OrganizationCreateDTO
     :> "organizations"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_POST :: OrganizationCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_POST reqDto = runInUnauthService $ addTraceUuidHeader =<< createOrganization reqDto
