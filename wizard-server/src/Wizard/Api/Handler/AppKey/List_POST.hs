module Wizard.Api.Handler.AppKey.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.AppKey.AppKeyService
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateJM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] AppKeyCreateDTO
    :> "app-keys"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> AppKeyCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mTokenHeader mServerUrl mUserAgent reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createAppKey reqDto mUserAgent
