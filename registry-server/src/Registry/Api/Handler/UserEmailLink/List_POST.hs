module Registry.Api.Handler.UserEmailLink.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.UserEmailLink.UserEmailLinkJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Model.UserEmailLink.UserEmailLinkType
import Registry.Service.Organization.OrganizationService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkJM ()

type List_POST =
  ReqBody '[SafeJSON] (UserEmailLinkDTO UserEmailLinkType)
    :> "user-email-links"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: UserEmailLinkDTO UserEmailLinkType -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST reqDto =
  runInUnauthService Transactional $
    addTraceUuidHeader =<< do
      resetOrganizationToken reqDto
      return NoContent
