module Wizard.Api.Handler.Token.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Util.Token
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.UserToken.UserTokenListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.UserToken
import Wizard.Model.User.UserTokenList
import Wizard.Service.UserToken.UserTokenService
import Wizard.Service.UserToken.UserTokenUtil

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
        let mCurrentTokenUuid = mTokenHeader >>= separateToken >>= getTokenUuidFromToken >>= U.fromString
        getTokens LoginUserTokenType mCurrentTokenUuid
