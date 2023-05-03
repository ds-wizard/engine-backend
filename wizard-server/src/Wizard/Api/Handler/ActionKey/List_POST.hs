module Wizard.Api.Handler.ActionKey.List_POST where

import Servant

import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.ActionKey.ActionKeyTypeJM ()
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] (ActionKeyDTO ActionKeyType)
    :> "action-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_POST :: Maybe String -> ActionKeyDTO ActionKeyType -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      resetUserPassword reqDto
      return NoContent
