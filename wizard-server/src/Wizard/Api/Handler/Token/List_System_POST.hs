module Wizard.Api.Handler.Token.List_System_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Token
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.System.SystemService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

type List_System_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> "tokens"
    :> "system"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_systemp_POST :: Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_systemp_POST mTokenHeader mServerUrl mUserAgent =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      let forbid = throwError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      case mTokenHeader of
        Just tokenHeader ->
          case separateToken tokenHeader of
            Just token -> createSystemToken token mUserAgent
            _ -> forbid
        _ -> forbid
