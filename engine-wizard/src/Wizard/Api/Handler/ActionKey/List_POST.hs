module Wizard.Api.Handler.ActionKey.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.ActionKey.ActionKeyJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_POST
   = ReqBody '[ SafeJSON] ActionKeyDTO
     :> "action-keys"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_POST :: ActionKeyDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_POST reqDto =
  runInUnauthService $
  addTraceUuidHeader =<< do
    resetUserPassword reqDto
    return NoContent
