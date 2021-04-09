module Registry.Api.Handler.Organization.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common

type Detail_GET
   = Header "Authorization" String
     :> "organizations"
     :> Capture "orgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
detail_GET mTokenHeader orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getOrganizationByOrgId orgId
