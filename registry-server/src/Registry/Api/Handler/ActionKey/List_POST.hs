module Registry.Api.Handler.ActionKey.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Model.ActionKey.ActionKeyType
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type List_POST =
  ReqBody '[SafeJSON] (ActionKeyDTO ActionKeyType)
    :> "action-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: ActionKeyDTO ActionKeyType -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST reqDto =
  runInUnauthService Transactional $
    addTraceUuidHeader =<< do
      resetOrganizationToken reqDto
      return NoContent
