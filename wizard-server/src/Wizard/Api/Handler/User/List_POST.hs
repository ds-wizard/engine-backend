module Wizard.Api.Handler.User.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserCreateJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserCreateDTO
    :> "users"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_POST
  :: Maybe String -> Maybe String -> UserCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        ia <- isAdmin
        if ia
          then createUserByAdmin reqDto
          else registerUser reqDto
