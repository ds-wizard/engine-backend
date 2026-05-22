module Wizard.Api.Handler.UserEmailLink.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.UserEmailLink.UserEmailLinkTypeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.User.UserService

type List_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] (UserEmailLinkDTO UserEmailLinkType)
    :> "user-email-links"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: Maybe String -> UserEmailLinkDTO UserEmailLinkType -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      resetUserPassword reqDto
      return NoContent
