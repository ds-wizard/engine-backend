module Wizard.Api.Handler.User.List_Current_Token_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.UserTokenService

type List_Current_Token_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "token"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_token_PUT
  :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_token_PUT mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteTokenByValue mTokenHeader
        return NoContent
