module Wizard.Api.Handler.Token.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenCreateJM ()
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Api.Resource.UserToken.UserTokenJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.UserTokenService

type List_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] UserTokenCreateDTO
    :> "tokens"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_POST :: Maybe String -> UserTokenCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< createTokenFromCredentials reqDto
