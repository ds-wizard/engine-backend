module Wizard.Api.Handler.Token.List_Current_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.Login.LoginService

type List_Current_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "tokens"
    :> "current"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_DELETE
  :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteLoginTokenByValue mTokenHeader
        return NoContent
