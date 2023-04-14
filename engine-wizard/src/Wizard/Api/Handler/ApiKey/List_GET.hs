module Wizard.Api.Handler.ApiKey.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.UserToken.UserTokenListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.UserToken
import Wizard.Model.User.UserTokenList
import Wizard.Service.UserToken.UserTokenService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "api-keys"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserTokenList])

list_GET
  :: Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [UserTokenList])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTokens ApiKeyUserTokenType Nothing
