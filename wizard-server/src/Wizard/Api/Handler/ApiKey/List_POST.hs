module Wizard.Api.Handler.ApiKey.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.ApiKey.ApiKeyService
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateJM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] ApiKeyCreateDTO
    :> "api-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> ApiKeyCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mTokenHeader mServerUrl mUserAgent reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createApiKey reqDto mUserAgent
