module Registry.Api.Handler.Organization.List_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService

type List_GET
   = Header "Authorization" String
     :> "organizations"
     :> Get '[ JSON] (Headers '[ Header "x-trace-uuid" String] [OrganizationDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [OrganizationDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getOrganizations
