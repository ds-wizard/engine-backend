module Registry.Api.Handler.Organization.Detail_DELETE where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Api.Handler.Common

type Detail_DELETE
   = Header "Authorization" String
     :> "organizations"
     :> Capture "orgId" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader orgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteOrganization orgId
      return NoContent
