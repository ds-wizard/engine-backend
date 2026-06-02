module Wizard.Api.Handler.User.List_Current_Email_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_Current_Email_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "email"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_email_PUT
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_email_PUT mTokenHeader mServerUrl hash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        confirmEmailChange hash
        return NoContent
