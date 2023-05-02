module Wizard.Api.Handler.Token.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.Login.LoginService
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.LoginJM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

type List_POST =
  Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] LoginDTO
    :> "tokens"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST :: Maybe String -> Maybe String -> LoginDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mServerUrl mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< createLoginTokenFromCredentials reqDto mUserAgent
