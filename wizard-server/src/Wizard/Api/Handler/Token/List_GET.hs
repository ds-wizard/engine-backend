module Wizard.Api.Handler.Token.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.Token
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import WizardLib.Public.Api.Resource.UserToken.UserTokenListJM ()
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Model.User.UserTokenList
import WizardLib.Public.Service.UserToken.UserTokenService
import WizardLib.Public.Service.UserToken.UserTokenUtil

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tokens"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserTokenList])

list_GET
  :: Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [UserTokenList])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mCurrentTokenUuid = mTokenHeader >>= separateToken >>= getTokenUuidFromToken
        getTokens LoginUserTokenType mCurrentTokenUuid
