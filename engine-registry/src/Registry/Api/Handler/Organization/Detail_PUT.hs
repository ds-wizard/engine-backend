module Registry.Api.Handler.Organization.Detail_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationChangeJM ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ JSON] OrganizationChangeDTO
     :> "organizations"
     :> Capture "orgId" String
     :> Put '[ JSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

detail_PUT ::
     Maybe String
  -> OrganizationChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
detail_PUT mTokenHeader reqDto orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyOrganization orgId reqDto
