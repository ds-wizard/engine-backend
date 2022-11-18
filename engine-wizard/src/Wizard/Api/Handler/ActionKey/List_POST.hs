module Wizard.Api.Handler.ActionKey.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.ActionKey.ActionKeyJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] ActionKeyDTO
    :> "action-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: Maybe String -> ActionKeyDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      resetUserPassword reqDto
      return NoContent
