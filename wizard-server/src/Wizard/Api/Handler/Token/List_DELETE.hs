module Wizard.Api.Handler.Token.List_DELETE where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Token
import Wizard.Api.Handler.Common
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.BaseContext
import WizardLib.Public.Service.UserToken.UserTokenService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "tokens"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let forbid = throwError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
        case mTokenHeader of
          Just tokenHeader ->
            case separateToken tokenHeader of
              Just token -> do
                deleteTokensExceptCurrentSession token
                return NoContent
              _ -> forbid
          _ -> forbid
