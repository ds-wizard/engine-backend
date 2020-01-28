module Registry.Api.Handler.ActionKey.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService

type List_POST
   = ReqBody '[ JSON] ActionKeyDTO
     :> "action-keys"
     :> Verb 'POST 201 '[ JSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_POST :: ActionKeyDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_POST reqDto =
  runInUnauthService $
  addTraceUuidHeader =<< do
    resetOrganizationToken reqDto
    return NoContent
